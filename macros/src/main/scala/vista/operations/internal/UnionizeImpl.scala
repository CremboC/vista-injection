package vista.operations.internal

import vista.operations.parsers.UnionizeInput
import vista.semantics

import scala.meta._
import scala.meta.contrib._

/**
  * Internal API implementation
  */
private[operations] object UnionizeImpl {
  def apply(inp: UnionizeInput)(implicit db: semantics.Database.type): Term.Block = {
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