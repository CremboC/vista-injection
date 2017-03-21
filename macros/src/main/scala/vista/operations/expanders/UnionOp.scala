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

    val leftTypeCtor  = Ctor.Name(inp.lclass)
    val rightTypeCtor = Ctor.Name(inp.rclass)

    val lsignatures = db.get(inp.lclass).methods.signatures
    val rsignatures = db.get(inp.rclass).methods.signatures

    val common = commonMethods(inp, lsignatures, rsignatures)
      .to[Seq]
      .sortBy(_.name.syntax)

    inp.newvar match {
      case None =>
        q"""
            trait $traitName extends $leftTypeCtor with $rightTypeCtor {
              ..$common
            }
            new ${Ctor.Name(traitName.value)} {}
        """

      case Some(nvar) =>
        q"""
            trait $traitName extends $leftTypeCtor with $rightTypeCtor {
              ..$common
            }
            val ${Term.Name(nvar).asPat} = new ${traitName.asCtorRef} {}
        """
    }
  }
}
