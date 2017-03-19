package vista

import scala.annotation.StaticAnnotation
import scala.meta.Term.Block
import scala.meta._
import vista.helpers.OpHelpers._
import vista.modifiers._
import vista.operations._
import vista.operations.parsers.OpOverload

import scala.collection.immutable.Seq

class enable extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    defn match {
      case obj: Defn.Object =>
        val template"{ ..$_ } with ..$_ { $_ => ..$stats }" = obj.templ

        // build up SemDB
        val db = semantics.Database
        val semDbBuilder: PartialFunction[Tree, Unit] = {
          case c: Defn.Class => db.addClass(c)
          case t: Defn.Trait => db.addClass(t)
        }

        defn.collect(semDbBuilder)

        val modifiers = Seq(
          ForbidModifiers.defnValModifier,
          UnionModifiers.defnValModifier,
          IntersectModifiers.defnValModifier,
          ProductModifiers.defnValModifier
        ).reduce(_ orElse _)

        val Block(nstats) = Block(stats)
          .transform { // first convert all classes into traits
            case classdefn: Defn.Class => Tratify(classdefn)
          }
          .transform {
            case v: Defn.Val => v.rhs match {
                case rhs: Term.New =>
                  val nCtors = rhs.templ.parents :+ Ctor.Name("vistas.AnyV")
                  v.copy(rhs = rhs.copy(rhs.templ.copy(parents = nCtors)))
                case _ => v
              }

            case b: Term.Block if hasOp(b) =>
              println(db.classes.map(_.name))


              val default: PartialFunction[Tree, Term.Block] = {
                case o: Stat => Term.Block(Seq(o))
              }
              val function = modifiers orElse default andThen { c =>
                c.collect(semDbBuilder)
                c
              }
              val modified = b.stats.collect(function).flatMap(_.stats)

              println(modified)

              Term.Block(modified)
          }

        val ntemplate = obj.templ.copy(stats = Option(nstats))
        obj.copy(templ = ntemplate)
      case _ =>
        abort("Only allowed on objects for now.")
    }
  }
}

