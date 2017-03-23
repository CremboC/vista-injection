package vista

import vista.helpers.OpHelpers
import vista.helpers.OpHelpers._
import vista.meta.xtensions.XTemplate
import vista.modifiers._
import vista.operations._

import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq
import scala.meta.Term.Block
import scala.meta._

class enable extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    defn match {
      case obj: Defn.Object =>
        val template"{ ..$_ } with ..$_ { $_ => ..$stats }" = obj.templ

        // build up SemDB
        val db = semantics.Database
        val semDbBuilder: PartialFunction[Tree, Unit] = {
          case c: Defn.Class => db.add(c)
          case t: Defn.Trait => db.add(t)
        }

        defn.traverse(semDbBuilder)

        // all modifiers that can expand a [A <: Tree] into a Term.Block
        // merge them into a single partial function so we can apply them at
        // the "same time"
        val modifiers = Seq(
          ForbidModifiers.valOverloadModifier,
          ForbidModifiers.valVistasModifier,
          UnionModifiers.valVistasModifier,
          IntersectModifiers.valVistasModifier,
          ProductModifiers.valVistasModifier
        ).reduce(_ orElse _)

        // if non of the modifiers match, we simply convert
        // a single statement into a term.block so we can use
        // flatMap later on
        val default: PartialFunction[Tree, Term.Block] = {
          case o: Stat => Term.Block(Seq(o))
        }

        // the final function that is used to expand ops
        // if none of the modifiers match, we used the default.
        // after the modifiers are applied, we add the newly generated
        // classes into the semdb so we can operate on them as well
        val function = modifiers orElse default andThen { c => c.traverse(semDbBuilder); c }

        def convertCtor(term: Term.New): Term.New = {
          val ctors = term.templ.parents :+ Ctor.Name("vistas.AnyV")
          term.copy(term.templ.copy(parents = ctors))
        }

        def instantiationRequiresConversion(term: Term.New): Boolean =
          if (term.templ.parents.isEmpty) false
          else {
            val className = term.templ.parents.head.syntax.takeWhile(_ != '(')
            db.exists(className)
          }

        def inheritsVistaTrait(term: Term.New): Boolean =
          if (term.templ.parents.isEmpty) false
          else {
            term.templ.parents.map(_.syntax).contains("vistas.AnyV")
          }

        def constructingTrait(term: Term.New): Boolean = term.templ.ctorsWithArguments.isDefined

        val Block(nstats) = Block(stats)
          .transform { // first convert all classes into traits
            case classdefn: Defn.Class if !OpHelpers.hasCaseMod(classdefn) => Tratify(classdefn)
          }
          .transform {
            // since we converted classes into traits we need to make sure they are instantiable
            case term: Term.New if instantiationRequiresConversion(term) && !inheritsVistaTrait(term) =>
              convertCtor(term)

            case term: Term.New if instantiationRequiresConversion(term) && constructingTrait(term) =>
              Tratify(term)

            case v: Defn.Val =>
              v.rhs match {
                case term: Term.New if !inheritsVistaTrait(term) => v.copy(rhs = convertCtor(term))
                case _ => v
              }

            // if a term block as an op, we will expand it
            case b: Term.Block if hasOp(b) =>
              val modified = b.stats.collect(function).flatMap(_.stats)
              Term.Block(modified)
          }

        val ntemplate = obj.templ.copy(stats = Option(nstats))
        obj.copy(templ = ntemplate)
      case _ =>
        abort("Only objects are supported")
    }
  }
}
