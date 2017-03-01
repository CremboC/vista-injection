package vista

import scala.annotation.StaticAnnotation
import scala.meta._

class union extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    val q"..$mods val $paramname: $tpeopt = $expr" = defn

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
    val className = Type.Name(s"${unionTyp.toString}c")

    val leftTypeCtor = Ctor.Name(leftType.toString)
    val rightTypeCtor = Ctor.Name(rightType.toString)

    val vrr = Pat.Var.Term(Term.Name(paramname.toString))
    q"""
        trait $traitName extends $leftTypeCtor with $rightTypeCtor
        class $className extends ${Ctor.Name(traitName.value)} {}
        ..$mods val $vrr = new ${Ctor.Name(className.value)}()
    """
  }
}
