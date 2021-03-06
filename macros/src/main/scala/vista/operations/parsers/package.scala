package vista.operations

import scala.annotation.implicitNotFound
import scala.meta._

package object parsers {
  @implicitNotFound("No parser found from ${I} to ${O}")
  trait Parser[I <: Tree, O <: OpInput] {
    def parse(defn: I): O
  }

  object Parser {

    private def parseType(typ: Type): String =
      if (typ.syntax.contains(".")) typ.syntax.split("""\.""").last
      else typ.syntax

    implicit object TermApplyOverloadParser extends Parser[Term.Apply, OpOverload] {
      override def parse(defn: Term.Apply): OpOverload = {
        val q"$_[$source, $result](${lvar: Term.Name}, $defs)" = defn

        val methods = {
          val q"..$stats" = defs
          stats.collect {
            case d: Defn.Def => d
          }
        }

        OpOverload(parseType(source), lvar.syntax, result.syntax, methods)
      }
    }

    implicit object TermApplyVistasParser extends Parser[Term.Apply, OpVistas] {
      override def parse(defn: Term.Apply): OpVistas = {
        val q"$_[$leftType, $rightType, $newType](${leftVar: Term.Name}, ${rightVar: Term.Name})" =
          defn

        OpVistas(parseType(leftType),
                 parseType(rightType),
                 leftVar.syntax,
                 rightVar.syntax,
                 newType.syntax)
      }
    }

    def apply[I <: Tree, O <: OpInput](implicit parser: Parser[I, O]): Parser[I, O] = parser
  }
}
