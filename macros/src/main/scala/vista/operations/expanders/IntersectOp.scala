package vista.operations.expanders

import _root_.meta.xtensions._
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

  private implicit val db = semantics.Database

  val expander: Expander[OpVistas, Intersect] = (inp: OpVistas) => {
    val traitName = Type.Name(inp.newtype)

    val leftTypeCtor  = Ctor.Name(inp.lclass)
    val rightTypeCtor = Ctor.Name(inp.rclass)

    val lclazz = db(inp.lclass)
    val rclazz = db(inp.rclass)

    val lsignatures = lclazz.methods.signatures
    val rsignatures = rclazz.methods.signatures

    // we're only overriding the methods that are no longer allowed
    // mintersect will return a list of methods which are allowed, but
    // we can only disallow methods, hence that is used here
    val disjointMethods = lsignatures >+< rsignatures
    val forbidden = disjointMethods.map { m =>
      m.copy(body = q"throw new NoSuchMethodException", mods = m.mods :+ Mod.Override())
    }

    // get common signatures in order to avoid "trait X inherits conflicting members"
    val common = commonMethods(inp, lsignatures, rsignatures)
    val result = (forbidden ++ common).to[Seq].sortBy(_.name.syntax)

//    val members = lclazz match {
//      case c: Inst.Class => Tratify.combineCtorMembers(c)
//      case _ => Seq.empty
//    }

    inp.newvar match {
      case None => ???
      case Some(nvar) =>
        q"""
           trait $traitName extends $leftTypeCtor with $rightTypeCtor {
             ..$result
           }
           val ${Term.Name(nvar).asPat} = new ${traitName.asCtorRef} {}
        """
    }
  }
}
