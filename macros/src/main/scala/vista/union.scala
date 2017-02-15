package vista

import scala.annotation.StaticAnnotation
import scala.meta._

/**
  * @author paulius
  */
class union extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    val q"..$mods val $paramname: $tpeopt = $expr" = defn

//    println(expr.children)
    val (leftVar, leftType, rightVar, rightType) = expr.children match {
      case union :: left :: right :: Nil =>
        val q"$unionOp[$leftType, $rightType]" = union
        val q"${leftVar: Term.Name}" = left
        val q"${rightVar: Term.Name}" = right

        (leftVar, leftType, rightVar, rightType)
      case _ => throw new RuntimeException("Illegal")
    }

    val unionTyp = tpeopt.asInstanceOf[Option[Type]] match {
      case Some(typ) => typ
      case _ => throw new IllegalArgumentException("Type must be set.")
    }
    val unionName = Term.Name(unionTyp.toString)

//    val unionName = Term.fresh("union")
    val traitType = Type.fresh("union")

    val unionType = Type.Name(unionName.value)

    val leftName = Term.fresh("left")
    val rightName = Term.fresh("right")

    val unionCtor = Ctor.Name(unionName.value)

    val vrr = Pat.Var.Term(Term.Name(paramname.toString))
    q"""
        trait $traitType {
          implicit def toRight(xy: $unionType): $rightType = xy.$rightName
        }
        object $unionName extends ${Ctor.Name(traitType.value)} {
          implicit def toLeft(xy: $unionType): $leftType = xy.$leftName
        }
        class $unionType(val $leftName: $leftType, val $rightName: $rightType) extends vistas.Union {
          def this(xy : $unionType) = this(xy.$leftName, xy.$rightName)
        }
        val $vrr = new $unionCtor($leftVar, $rightVar)
     """
  }
}
