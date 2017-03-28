package vista.operations.expanders

import vista.meta.xtensions._
import vista.operations.parsers.OpVistas
import vista.semantics

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.contrib._

/**
  * @author Paulius Imbrasas
  */
private[operations] object IntersectOp {
  type Intersect = Op[IntersectOp.type]

  private val db = semantics.Database

  implicit object VistaExpander extends Expander[OpVistas, Intersect] {
    override def expand(inp: OpVistas): Term.Block = {
      val traitName = Type.Name(inp.newtype)

      val leftTypeCtor  = Ctor.Name(inp.lclass)
      val rightTypeCtor = Ctor.Name(inp.rclass)

      val lclazz = db(inp.lclass)
      val rclazz = db(inp.rclass)

      val lsignatures = lclazz.visibilities.signatures
      val rsignatures = rclazz.visibilities.signatures

      // we're only overriding the methods that are no longer allowed
      // mintersect will return a list of methods which are allowed, but
      // we can only disallow methods, hence that is used here
      val disjointMethods = lsignatures <-> rsignatures
      val forbidden = disjointMethods.map { m =>
        m.copy(body = q"throw new NoSuchMethodException", mods = m.mods :+ Mod.Override())
      }

      // get common signatures in order to avoid "trait X inherits conflicting members"
      val common = commonMethods(inp)
      val result = (forbidden ++ common).to[Seq].sortBy(_.name.syntax)

      val members = ctorMembersDefns(lclazz, inp.lvar) ++ ctorMembersDefns(rclazz, inp.rvar)

      inp.newvar match {
        case None => ???
        case Some(nvar) =>
          q"""
           trait $traitName extends $leftTypeCtor with $rightTypeCtor {
             ..$result
           }
           val ${Term.Name(nvar).asPat} = new ${traitName.asCtorRef} {
             ..$members
           }
        """
      }
    }
  }
}
