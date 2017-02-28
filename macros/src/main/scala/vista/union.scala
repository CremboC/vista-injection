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


    val leftTypeCtor = Ctor.Name(leftType.toString)
    val rightTypeCtor = Ctor.Name(rightType.toString)


//    val template = {
//      Seq(ctor"$leftTypeCtor", ctor"$rightTypeCtor")
//    }
//    Ctor.Call(leftType.toString)
//    template"{} with ..$template"


//    val template = template"{ ..${Seq.empty} } with ..$ctorcalls"
    q"""
        class $unionType extends $leftTypeCtor with $rightTypeCtor
        val $vrr = new $unionCtor()
    """
  }
}
