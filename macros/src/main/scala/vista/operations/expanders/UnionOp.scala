package vista.operations.expanders

import meta.xtensions.XDefnIterable
import vista.operations.parsers.OpVistas
import vista.semantics

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.contrib._

/**
  * Internal API implementation
  */
private[operations] object UnionOp {
  type Union = Op[UnionOp.type]

  private implicit val db = semantics.Database

  val expander: Expander[OpVistas, Union] = (inp: OpVistas) => {
    val traitName = Type.Name(inp.newtype)

    val lctor  = Ctor.Name(inp.lclass)
    val rctor = Ctor.Name(inp.rclass)

    val lclazz = db.get(inp.lclass)
    val rclazz = db.get(inp.rclass)

    val lsignatures = lclazz.methods.signatures
    val rsignatures = rclazz.methods.signatures

    val common = commonMethods(inp, lsignatures, rsignatures)
      .to[Seq]
      .sortBy(_.name.syntax)

    val members = ctorMembersDefns(lclazz, inp.lvar) ++ ctorMembersDefns(rclazz, inp.rvar)

    inp.newvar match {
      case None =>
        q"""
            trait $traitName extends $lctor with $rctor {
              ..$common
            }
            new ${Ctor.Name(traitName.value)} {
              ..$members
            }
        """

      case Some(nvar) =>
        q"""
            trait $traitName extends $lctor with $rctor {
              ..$common
            }
            val ${Term.Name(nvar).asPat} = new ${traitName.asCtorRef} {
              ..$members
            }
        """
    }
  }
}
