package vista

import vista.Constants.forbiddenMethodBody
import vista.helpers.OpHelpers
import vista.modifiers._
import vista.operations._
import vista.operations.expanders._
import vista.operations.parsers.{OpOverload, OpVistas, Parser}
import vista.util.meta.xtensions.XTemplate

import scala.annotation.StaticAnnotation
import scala.meta.Term.Block
import scala.meta._
import scala.meta.contrib._

class enable extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val db = semantics.Database

    def classIsRecorded(term: Term.New): Boolean =
      if (term.templ.parents.isEmpty) false
      else {
        val className = term.templ.parents.head.syntax.takeWhile(_ != '(')
        db.exists(className)
      }

    def constructingTrait(term: Term.New): Boolean = term.templ.ctorsWithArguments.isDefined

    defn match {
      case obj: Defn.Object =>
        val template"{ ..$_ } with ..$_ { $_ => ..$stats }" = obj.templ

        // build up SemDB
        defn.traverse {
          case c: Defn.Class => db.add(c)
          case t: Defn.Trait => db.add(t)
        }

        val generated = defn.collect {
          case OpHelpers.HasOp(term) =>
            val parser = term match {
              case OpHelpers.OpVistas() =>
                term match {
                  case OpHelpers.Forbid(_) =>
                    parseAndExpand[Term.Apply, OpVistas, ForbidOp.Forbid] _
                  case OpHelpers.Union(_) =>
                    parseAndExpand[Term.Apply, OpVistas, UnionOp.Union] _
                  case OpHelpers.Intersect(_) =>
                    parseAndExpand[Term.Apply, OpVistas, IntersectOp.Intersect] _
                  case OpHelpers.Product(_) =>
                    parseAndExpand[Term.Apply, OpVistas, ProductOp.Product] _
                }
              case OpHelpers.OpOverload() =>
                term match {
                  case OpHelpers.Forbid(_) =>
                    parseAndExpand[Term.Apply, OpOverload, ForbidOp.Forbid] _
//                case OpHelpers.Intersect(_) =>
//                  parseAndExpand[Term.Apply, OpOverload, IntersectOp.Intersect] _
                }
            }

            (parser, term)
        }

        val addGenerated = (g: Defn.Trait) => { db.add(g, generated = true); g }
        val traits = generated.collect {
          case (f, t) =>
            val trayt = f andThen addGenerated apply t
            trayt
              .transform {
                case d: Defn.Def if d.body isEqual forbiddenMethodBody =>
                  d.copy(mods = restrictAnnotation(d.name.value, trayt.name.value) +: d.mods)
              }
              .asInstanceOf[Defn.Trait]
        }

        val generatedWrapper = q"object ${Term.Name(Constants.GenName)} { ..$traits }"
        val generatedImport  = q"import ${Term.Name(Constants.GenName)}._"

        val Block(nstats) = Block(stats)
          .transform { // first convert all classes into traits
            case classdefn: Defn.Class if !OpHelpers.hasCaseMod(classdefn) => Tratify(classdefn)
          }
          .transform {
            case term: Term.New if classIsRecorded(term) && constructingTrait(term) =>
              Tratify(term)

            case OpHelpers.Subset(t) =>
              Subset(t)

            case OpHelpers.HasOp(t) =>
              val parser = t match {
                case OpHelpers.OpVistas()   => Parser[Term.Apply, OpVistas]
                case OpHelpers.OpOverload() => Parser[Term.Apply, OpOverload]
              }

              val op = parser.parse(t)

              val ctorable = t match {
                case OpHelpers.Forbid(_)    => Constructable[ForbidOp.Forbid]
                case OpHelpers.Union(_)     => Constructable[UnionOp.Union]
                case OpHelpers.Intersect(_) => Constructable[IntersectOp.Intersect]
                case OpHelpers.Product(_)   => Constructable[ProductOp.Product]
              }

              val members = ctorable.members(op)
              q"new ${db.ctor(op.newtype)} { ..$members }"
          }

        val unparsed = nstats.find {
          case s: Stat if OpHelpers.hasOp(s) => true
          case _                             => false
        }

        unparsed match {
          case Some(s) => abort(s"An operation was not expanded in $s (${s.pos})")
          case None    =>
        }

        val ntemplate =
          obj.templ.copy(stats = Option(generatedWrapper +: generatedImport +: nstats))
        obj.copy(templ = ntemplate)
      case _ =>
        abort("Only objects are supported")
    }
  }
}
