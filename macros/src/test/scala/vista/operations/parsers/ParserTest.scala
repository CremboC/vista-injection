package vista.operations.parsers

import org.scalatest.{Inside, OptionValues}
import vista.FlatSpecBase

import scala.meta._

class ParserTest extends FlatSpecBase with Inside with OptionValues {
  behavior of "Parser"

//  private val valParser          = Parser[Defn.Val, OpVistas].parse _
//  private val valOverloadParser  = Parser[Defn.Val, OpOverload].parse _
//  private val defnParser         = Parser[Defn.Def, OpVistas].parse _
//  private val defnOverloadParser = Parser[Defn.Def, OpOverload].parse _

  it should "parse a method application of two vistas" in {
    val parsed = Parser[Term.Apply, OpVistas].parse(q"∪[A & B ~> AB](a, b)")
    parsed should not be empty
    inside(parsed.get) {
      case OpVistas(lclass, rclass, lvar, rvar, newtype, nvar) =>
        lclass should be("A")
        rclass should be("B")
        lvar should be("a")
        rvar should be("b")
        newtype should be("AB")
        nvar shouldBe empty
    }
  }

  it should "fail to parse a method application of two vistas when the result is missing" in {
    assertThrows[MatchError] {
      Parser[Term.Apply, OpVistas].parse(q"∪[A & B](a, b)")
    }
  }

  it should "fail to parse a method application of two vistas when one of the types is missing" in {
    assertThrows[MatchError] {
      Parser[Term.Apply, OpVistas].parse(q"∪[A ~> AB](a, b)")
    }
  }

  it should "parse a method application of a vista and a set of names" in {
    val parsed = Parser[Term.Apply, OpOverload].parse(q"∖[A ~> Af](a, { def a: Int = ??? })")
    parsed should not be empty
    inside(parsed.get) {
      case OpOverload(lclass, lvar, newtype, methods, nvar) =>
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

//  it should "parse a val definition of two vistas of union" in {
//    val parsed = valParser(q"val ab: AB = ∪[A, B](a, b)")
//    parsed should not be empty
//    inside(parsed.get) {
//      case OpVistas(lclass, rclass, lvar, rvar, newtype, nvar) =>
//        lclass should be("A")
//        rclass should be("B")
//        lvar should be("a")
//        rvar should be("b")
//        newtype should be("AB")
//        nvar.value should be("ab")
//    }
//  }
//
//  it should "parse a val definition of two vistas of difference" in {
//    val parsed = valParser(q"val ab: AB = ∖[A, B](a, b)")
//    parsed should not be empty
//    inside(parsed.get) {
//      case OpVistas(lclass, rclass, lvar, rvar, newtype, nvar) =>
//        lclass should be("A")
//        rclass should be("B")
//        lvar should be("a")
//        rvar should be("b")
//        newtype should be("AB")
//        nvar.value should be("ab")
//    }
//  }
//
//  it should "parse a val definition of two vistas of intersection" in {
//    val parsed = valParser(q"val ab: AB = ∩[A, B](a, b)")
//    parsed should not be empty
//    inside(parsed.get) {
//      case OpVistas(lclass, rclass, lvar, rvar, newtype, nvar) =>
//        lclass should be("A")
//        rclass should be("B")
//        lvar should be("a")
//        rvar should be("b")
//        newtype should be("AB")
//        nvar.value should be("ab")
//    }
//  }
//
//  it should "parse a val definition of two vistas of product" in {
//    val parsed = valParser(q"val ab: AB = ⨯[A, B](a, b)")
//    parsed should not be empty
//    inside(parsed.get) {
//      case OpVistas(lclass, rclass, lvar, rvar, newtype, nvar) =>
//        lclass should be("A")
//        rclass should be("B")
//        lvar should be("a")
//        rvar should be("b")
//        newtype should be("AB")
//        nvar.value should be("ab")
//    }
//  }
//
//  it should "parse a method definition of two vistas" in {
//    val parsed = defnParser(q"def ab(a: A, b: B): AB = ∪[A, B](a, b)")
//    parsed should not be empty
//    inside(parsed.get) {
//      case OpVistas(lclass, rclass, lvar, rvar, newtype, nvar) =>
//        lclass should be("A")
//        rclass should be("B")
//        lvar should be("a")
//        rvar should be("b")
//        newtype should be("AB")
//        nvar shouldBe empty
//    }
//  }
//
//  it should "parse a method definition of a vista and a set of names" in {
//    val parsed = defnOverloadParser(q"def ab(a: A): AB = ∖[A](a, { def a: Int = ??? })")
//    parsed should not be empty
//    inside(parsed.get) {
//      case OpOverload(lclass, lvar, newtype, methods, nvar) =>
//        lclass should be("A")
//        lvar should be("a")
//        newtype should be("AB")
//        methods should contain only q"def a: Int = ???"
//        nvar shouldBe empty
//    }
//  }
//
//  it should "fail when a type is missing" in {
//    assertThrows[MatchError] {
//      valParser(q"val ab: AB = ⨯[A](a, b)")
//    }
//  }
//
//  it should "fail when a the return type is missing" in {
//    assertThrows[MatchError] {
//      valParser(q"val ab = ⨯[A, B](a, b)")
//    }
//  }
//
//  it should "fail when a one of the parameters is missing" in {
//    assertThrows[MatchError] {
//      valParser(q"val ab: AB = ⨯[A, B](a)")
//    }
//  }
}
