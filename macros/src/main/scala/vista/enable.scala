package vista

import scala.annotation.StaticAnnotation
import scala.meta.Term.Block
import scala.meta._
import vista.helpers.OpHelpers._
import vista.modifiers._
import vista.operations._

import scala.collection.immutable.Seq

class enable extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    defn match {
      case obj: Defn.Object =>
        val template"{ ..$_ } with ..$_ { $_ => ..$stats }" = obj.templ

        // build up SemDB
        implicit val db = semantics.Database
        defn.collect {
          case c: Defn.Class => db.addClass(c)
        }

        val ops = Seq(Forbid, Unionize)
        val modifiers = ops.map(_.modifier).reduce(_ orElse _)

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
              val modified = b.stats.collect(modifiers orElse {
                case o => Term.Block(Seq(o))
              }).flatMap(_.stats)

              Term.Block(modified)
          }

        val ntemplate = obj.templ.copy(stats = Option(nstats))
        obj.copy(templ = ntemplate)
      case _ =>
        abort("Only allowed on objects for now.")
    }
  }
}

