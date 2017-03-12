package vista.operations.internal

import vista.operations.UnionizeInput
import vista.semantics

import scala.meta._
import scala.meta.contrib._

/**
  * Internal API implementation
  */
private[operations] object UnionizeImpl {
  def parseDefn(defn: Defn.Val): Option[UnionizeInput] = defn.decltpe match {
    case None => None
    case Some(typ) =>
      val (leftVar, leftType, rightVar, rightType) = defn.rhs.children match {
        case union :: left :: right :: Nil =>
          val q"$_[$leftType, $rightType]" = union
          val q"${leftVar: Term.Name}" = left
          val q"${rightVar: Term.Name}" = right

          (leftVar, leftType, rightVar, rightType)
        case _ => throw new RuntimeException("Illegal")
      }

      Some(UnionizeInput(leftType.syntax, rightType.syntax,
        leftVar.syntax, rightVar.syntax,
        typ.syntax, Some(defn.pats.head.syntax)))
  }

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
