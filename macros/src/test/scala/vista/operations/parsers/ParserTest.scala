package vista.operations.parsers

import org.scalatest.{Inside, OptionValues}
import vista.FlatSpecBase

import scala.meta._

class ParserTest extends FlatSpecBase with Inside with OptionValues {
  behavior of "Parser"

  private val valParser = Parser[Defn.Val, OpVistas].parse _

  it should "parse a val definition of two vistas of union" in {
    val parsed = valParser(q"val ab: AB = ∪[A, B](a, b)")
    parsed should not be empty
    inside(parsed.get) {
      case OpVistas(lclass, rclass, lvar, rvar, newtype, nvar) =>
        lclass should be("A")
        rclass should be("B")
        lvar should be("a")
        rvar should be("b")
        newtype should be("AB")
        nvar.value should be ("ab")
    }
  }

  it should "parse a val definition of two vistas of difference" in {
    val parsed = valParser(q"val ab: AB = ∖[A, B](a, b)")
    parsed should not be empty
    inside(parsed.get) {
      case OpVistas(lclass, rclass, lvar, rvar, newtype, nvar) =>
        lclass should be("A")
        rclass should be("B")
        lvar should be("a")
        rvar should be("b")
        newtype should be("AB")
        nvar.value should be ("ab")
    }
  }

  it should "parse a val definition of two vistas of intersection" in {
    val parsed = valParser(q"val ab: AB = ∩[A, B](a, b)")
    parsed should not be empty
    inside(parsed.get) {
      case OpVistas(lclass, rclass, lvar, rvar, newtype, nvar) =>
        lclass should be("A")
        rclass should be("B")
        lvar should be("a")
        rvar should be("b")
        newtype should be("AB")
        nvar.value should be ("ab")
    }
  }

  it should "parse a val definition of two vistas of product" in {
    val parsed = valParser(q"val ab: AB = ⨯[A, B](a, b)")
    parsed should not be empty
    inside(parsed.get) {
      case OpVistas(lclass, rclass, lvar, rvar, newtype, nvar) =>
        lclass should be("A")
        rclass should be("B")
        lvar should be("a")
        rvar should be("b")
        newtype should be("AB")
        nvar.value should be ("ab")
    }
  }

  it should "fail when a type is missing" in {
    assertThrows[MatchError] {
      valParser(q"val ab: AB = ⨯[A](a, b)")
    }
  }

  it should "fail when a the return type is missing" in {
    assertThrows[MatchError] {
      valParser(q"val ab = ⨯[A, B](a, b)")
    }
  }

  it should "fail when a one of the parameters is missing" in {
    assertThrows[MatchError] {
      valParser(q"val ab: AB = ⨯[A, B](a)")
    }
  }
}
