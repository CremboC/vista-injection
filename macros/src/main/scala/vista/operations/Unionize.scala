package vista.operations

import vista.semantics

import scala.meta._

object Unionize {
  def apply(definition: Defn.Val)(implicit db: semantics.Database.type): Tree = {
    val q"..$mods val $paramname: $tpeopt = $expr" = definition

    val (_, leftType, _, rightType) = expr.children match {
      case union :: left :: right :: Nil =>
        val q"$_[$leftType, $rightType]" = union
        val q"${leftVar: Term.Name}" = left
        val q"${rightVar: Term.Name}" = right

        (leftVar, leftType, rightVar, rightType)
      case _ => throw new RuntimeException("Illegal")
    }

    val unionTyp = tpeopt.asInstanceOf[Option[Type]].getOrElse {
      throw new IllegalArgumentException("Type must be set.")
    }

    val traitName = Type.Name(unionTyp.toString)

    val leftTypeCtor = Ctor.Name(leftType.toString)
    val rightTypeCtor = Ctor.Name(rightType.toString)

    val vrr = Pat.Var.Term(Term.Name(paramname.toString))
    q"""
        trait $traitName extends $leftTypeCtor with $rightTypeCtor
        ..$mods val $vrr = new ${Ctor.Name(traitName.value)} {}
    """
  }
}
