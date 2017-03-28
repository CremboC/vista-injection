package vista.operations

import scala.annotation.implicitNotFound
import scala.meta._

/**
  * @author Paulius
  */
package object parsers {
  @implicitNotFound("No parser found from ${I} to ${O}")
  trait Parser[I <: Tree, O <: OpInput] {
    def parse(defn: I): Option[O]
  }

  object Parser {

    /** Handles the parsing of a val definition with two type arguments
      * {{{val a: AB' = op[A, B](a, b)}}}
      */
    implicit object ValTwoVistasParser extends Parser[Defn.Val, OpVistas] {
      override def parse(defn: Defn.Val): Option[OpVistas] = {
        val typ = defn.decltpe.getOrElse { throw new MatchError("Must provide decltpe") }

        val (leftVar, leftType, rightVar, rightType) = defn.rhs.children match {
          case op :: left :: right :: Nil =>
            val q"$_[$leftType, $rightType]" = op
            val q"${leftVar: Term.Name}"     = left
            val q"${rightVar: Term.Name}"    = right

            (leftVar, leftType, rightVar, rightType)
          case _ => throw new MatchError("Missing argument for op")
        }

        Option(
          OpVistas(leftType.syntax,
                   rightType.syntax,
                   leftVar.syntax,
                   rightVar.syntax,
                   typ.syntax,
                   Option(defn.pats.head.syntax)))
      }
    }

    /** Handles the parsing of a val definition with a single type argument.
      * {{{val a: AB' = op[A](a, {...})}}}
      */
    implicit object ValOverloadParser extends Parser[Defn.Val, OpOverload] {
      override def parse(defn: Defn.Val): Option[OpOverload] = {
        val newtype = defn.decltpe.getOrElse {
          throw new MatchError("Must provide a type for the vista")
        }
        val q"$_[..$typargs](..$args)" = defn.rhs
        val lclass                     = typargs.head

        val lvar = args.head

        val methods = {
          val q"..$stats" = args.last
          stats.collect {
            case d: Defn.Def => d
          }
        }

        val paramname = defn.pats.head
        Option(
          OpOverload(lclass.syntax, lvar.syntax, newtype.syntax, methods, Some(paramname.syntax)))
      }
    }

    implicit object DefnVistaParser extends Parser[Defn.Def, OpVistas] {
      override def parse(defn: Defn.Def): Option[OpVistas] = {
        val newtype = defn.decltpe.getOrElse {
          throw new MatchError("Must provide a type for the vista")
        }

        val (leftVar, leftType, rightVar, rightType) = defn.body.children match {
          case op :: left :: right :: Nil =>
            val q"$_[$leftType, $rightType]" = op
            val q"${leftVar: Term.Name}"     = left
            val q"${rightVar: Term.Name}"    = right

            (leftVar, leftType, rightVar, rightType)
          case _ => throw new MatchError("Missing argument for op")
        }

        Option(
          OpVistas(leftType.syntax,
                   rightType.syntax,
                   leftVar.syntax,
                   rightVar.syntax,
                   newtype.syntax))
      }
    }

    implicit object DefnOverloadParser extends Parser[Defn.Def, OpOverload] {
      override def parse(defn: Defn.Def): Option[OpOverload] = {
        val newtype = defn.decltpe.getOrElse {
          throw new MatchError("Must provide a type for the vista")
        }
        val q"$_[..$typargs](..$args)" = defn.body
        val lclass                     = typargs.head

        val lvar = args.head

        val methods = {
          val q"..$stats" = args.last
          stats.collect {
            case d: Defn.Def => d
          }
        }

        Option(OpOverload(lclass.syntax, lvar.syntax, newtype.syntax, methods))
      }
    }

    def apply[I <: Tree, O <: OpInput](implicit parser: Parser[I, O]): Parser[I, O] = parser
  }
}
