package vista.operations.expanders

import vista.operations.parsers.OpVistas
import vista.semantics

import scala.meta._
import scala.meta.contrib._

/**
  * Internal API implementation
  */
private[operations] object UnionOp {
  type Union = Op[UnionOp.type ]

  private implicit val db = semantics.Database

  val expander: Expander[OpVistas, Union] = (inp: OpVistas) => {
    val traitName = Type.Name(inp.newtype)

    val leftTypeCtor = Ctor.Name(inp.lclass)
    val rightTypeCtor = Ctor.Name(inp.rclass)

    inp.newvar match {
      case None =>
        q"""
            trait $traitName extends $leftTypeCtor with $rightTypeCtor
            new ${Ctor.Name(traitName.value)} {}
        """

      case Some(nvar) =>
        q"""
            trait $traitName extends $leftTypeCtor with $rightTypeCtor
            val ${Term.Name(nvar).asPat} = new ${traitName.asCtorRef} {}
        """
    }
  }
}
