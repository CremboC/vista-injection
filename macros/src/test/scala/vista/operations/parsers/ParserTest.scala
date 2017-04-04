package vista.operations.parsers

import org.scalatest.{Inside, OptionValues}
import vista.FlatSpecBase

import scala.meta._

class ParserTest extends FlatSpecBase with Inside with OptionValues {
  behavior of "Parser"

  it should "parse a method application of two vistas" in {
    val parsed = Parser[Term.Apply, OpVistas].parse(q"∪[A, B, AB](a, b)")
    inside(parsed) {
      case OpVistas(lclass, rclass, lvar, rvar, newtype) =>
        lclass should be("A")
        rclass should be("B")
        lvar should be("a")
        rvar should be("b")
        newtype should be("AB")
    }
  }

  it should "fail to parse a method application of two vistas when the result is missing" in {
    assertThrows[MatchError] {
      Parser[Term.Apply, OpVistas].parse(q"∪[A, B](a, b)")
    }
  }

  it should "fail to parse a method application of two vistas when one of the types is missing" in {
    assertThrows[MatchError] {
      Parser[Term.Apply, OpVistas].parse(q"∪[A, AB](a, b)")
    }
  }

  it should "parse a method application of a vista and a set of names" in {
    val parsed = Parser[Term.Apply, OpOverload].parse(q"∖[A, Af](a, { def a: Int = ??? })")
    inside(parsed) {
      case OpOverload(lclass, lvar, newtype, methods) =>
        lclass should be("A")
        lvar should be("a")
        newtype should be("Af")
        methods should contain only q"def a: Int = ???"
    }
  }

  it should "fail to parse a method application of a vista and a set of names when the result is missing" in {
    assertThrows[MatchError] {
      Parser[Term.Apply, OpOverload].parse(q"∖[A](a, { def a: Int = ??? })")
    }
  }
}
