package vista

import scala.annotation.StaticAnnotation
import scala.meta._

/**
  * @author paulius
  */
class union extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    // using scala.meta because it is more statically typed than scala.reflect
    val q"..$mods val $paramname: $tpeopt = $expr" = defn

    println(expr.children)
    val (leftVar, leftType, rightVar, rightType) = expr.children match {
      case union :: left :: right :: Nil =>
        val q"$unionOp[$leftType, $rightType]" = union
        val q"${leftVar: Term.Name}" = left
        val q"${rightVar: Term.Name}" = right

        (leftVar, leftType, rightVar, rightType)
      case _ => throw new RuntimeException("Illegal")
    }

    val unionName = Term.fresh("union")
    val unionType = Type.Name(unionName.value)

    val leftName = Term.fresh("left")
    val rightName = Term.fresh("right")

    val unionCtor = Ctor.Name(unionName.value)

    val vrr = Pat.Var.Term(Term.Name(paramname.toString))
    q"""
        object $unionName {
          implicit def toLeft(xy: $unionType): $leftType = xy.$leftName
          implicit def toRight(xy: $unionType): $rightType = xy.$rightName
        }
        class $unionType(val $leftName: $leftType, val $rightName: $rightType)
        val $vrr = new $unionCtor($leftVar, $rightVar) with vistas.Vista
     """

    //        Macros.resolveConflicts(${Term.Name(paramname.toString)}, $leftVar, $rightVar)
  }
}
