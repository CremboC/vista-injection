package vista

import vista.helpers.OpHelpers
import vista.meta.xtensions.XTemplate
import vista.modifiers._
import vista.operations._
import vista.operations.expanders._
import vista.operations.parsers.{OpOverload, OpVistas, Parser}

import scala.annotation.StaticAnnotation
import scala.meta.Term.Block
import scala.meta._

class enable extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val db = semantics.Database

    def convertCtor(term: Term.New): Term.New = {
      val ctors = term.templ.parents :+ Ctor.Name(Constants.AnyV)
      term.copy(term.templ.copy(parents = ctors))
    }

    def classIsRecorded(term: Term.New): Boolean =
      if (term.templ.parents.isEmpty) false
      else {
        val className = term.templ.parents.head.syntax.takeWhile(_ != '(')
        db.exists(className)
      }

    def inheritsVistaTrait(term: Term.New): Boolean =
      if (term.templ.parents.isEmpty) false
      else {
        term.templ.parents.map(_.syntax).contains(Constants.AnyV)
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
          case t: Term.Apply if OpHelpers.hasOp(t) =>
            val parser = t match {
              case OpHelpers.Forbid(_)       => t match {
                  case OpHelpers.OpVistas()   => parseAndExpand[Term.Apply, OpVistas, ForbidOp.Forbid] _
                  case OpHelpers.OpOverload() => parseAndExpand[Term.Apply, OpOverload, ForbidOp.Forbid] _
                }
              case OpHelpers.Union(_)        => t match {
                  case OpHelpers.OpVistas()   => parseAndExpand[Term.Apply, OpVistas, UnionOp.Union] _
  //                case OpHelpers.OpOverload() => parseAndExpand[Term.Apply, OpOverload, UnionOp.Union](t)
                }
              case OpHelpers.Intersect(_) => t match {
                  case OpHelpers.OpVistas()   => parseAndExpand[Term.Apply, OpVistas, IntersectOp.Intersect] _
  //                case OpHelpers.OpOverload() => parseAndExpand[Term.Apply, OpOverload, IntersectOp.Intersect](t)
                }
              case OpHelpers.Product(_)      => t match {
                case OpHelpers.OpVistas()   => parseAndExpand[Term.Apply, OpVistas, ProductOp.Product] _
  //              case OpHelpers.OpOverload() => parseAndExpand[Term.Apply, OpOverload, ProductOp.Product](t)
              }
            }

            (parser, t)
        }

        val addGenerated = (g: Defn.Trait) => { db.add(g, generated = true); g }
        val traits = generated.collect {
          case (f, t) => f andThen addGenerated apply t
        }

        val generatedWrapper = q"object ${Term.Name(Constants.GenName)} { ..$traits }"
        val generatedImport = q"import ${Term.Name(Constants.GenName)}._"

        val Block(nstats) = Block(stats)
          .transform { // first convert all classes into traits
            case classdefn: Defn.Class if !OpHelpers.hasCaseMod(classdefn) => Tratify(classdefn)
          }
          .transform {
            // since we converted classes into traits we need to make sure they are instantiable
            case term: Term.New if classIsRecorded(term) && !inheritsVistaTrait(term) =>
              convertCtor(term)

            case term: Term.New if classIsRecorded(term) && constructingTrait(term) =>
              Tratify(term)

            case v: Defn.Val =>
              v.rhs match {
                case term: Term.New if !inheritsVistaTrait(term) => v.copy(rhs = convertCtor(term))
                case _ => v
              }

            case OpHelpers.Subset(t) =>
              Subset(t)

            case OpHelpers.HasOp(t) =>
              val parser = t match {
                case OpHelpers.OpVistas() => Parser[Term.Apply, OpVistas]
                case OpHelpers.OpOverload() => Parser[Term.Apply, OpOverload]
              }

              val op = parser.parse(t).getOrElse { abort("Fatal error occurred when parsing an op") }

              val ctorable = t match {
                case OpHelpers.Forbid(_) => Constructable[ForbidOp.Forbid]
                case OpHelpers.Union(_) => Constructable[UnionOp.Union]
                case OpHelpers.Intersect(_) => Constructable[IntersectOp.Intersect]
                case OpHelpers.Product(_) => Constructable[ProductOp.Product]
              }

              val members = ctorable.members(op)
              q"new ${db.ctor(op.newtype)} { ..$members }"
          }

        val ntemplate = obj.templ.copy(stats = Option(generatedWrapper +: generatedImport +: nstats))
//        println(ntemplate)
        obj.copy(templ = ntemplate)
      case _ =>
        abort("Only objects are supported")
    }
  }
}