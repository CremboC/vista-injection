package vista.operations

import scala.meta._

/**
  * @author Paulius
  */
package object parsers {
  trait Parser[From, To <: OpInput] {
    def parse(defn: From): Option[To]
  }

  implicit val defnValToNormal: Parser[Defn.Val, OpVistas] =
    (defn: Defn.Val) => defn.decltpe match {
      case None => None
      case Some(typ) =>
        val (leftVar, leftType, rightVar, rightType) = defn.rhs.children match {
          case op :: left :: right :: Nil =>
            val q"$_[$leftType, $rightType]" = op
            val q"${leftVar: Term.Name}" = left
            val q"${rightVar: Term.Name}" = right

            (leftVar, leftType, rightVar, rightType)
          case _ => throw new RuntimeException("Illegal")
        }

        Some(OpVistas(leftType.syntax, rightType.syntax,
          leftVar.syntax, rightVar.syntax,
          typ.syntax, Some(defn.pats.head.syntax)))
    }

  implicit val defnValToOverloaded: Parser[Defn.Val, OpOverload] =
    (defn: Defn.Val) => defn.decltpe match {
      case None => None
      case Some(newtype) =>
        val q"$_[..$typargs](..$args)" = defn.rhs
        val lclass = typargs.head

        val lvar = args.head

        val methods = {
          val q"..$stats" = args.last
          stats.collect {
            case d: Defn.Def => d
          }
        }

        val paramname = defn.pats.head
        Some(OpOverload(lclass.syntax, lvar.syntax, newtype.syntax, methods, Some(paramname.syntax)))
    }

  def parse[From, To <: OpInput](defn: From)(implicit parser: Parser[From, To]): Option[To] = parser.parse(defn)
}
