package vista.operations.expanders

import vista.FlatSpecBase
import vista.operations.parsers.OpVistas
import vista.util.Pipe._

import scala.meta._

class ExpandersTest extends FlatSpecBase {
  behavior of "expanders"

  "Common methods" should "produce the correct result with a simple case" in {
    val source =
      q"""
        class A {
          def a: Int = 1
        }

        class B {
          def a: Int = 2
        }
      """

    source |> addInsts

    val common = commonMethods(OpVistas("A", "B", "a", "b", "AB"))
    common should have size 1
    common.head.syntax should equal {
      q"override def a: Int = super[A].a".syntax
    }
  }

  "Common methods" should "produce the correct result with a complex case" in {
    val source =
      q"""
        class AB {
          def a: Int = 1
          def b: Int = 2
        }

        trait Aa extends AB {
          def a: Int = 1
          override def b: Int = ${vista.Constants.forbiddenMethodBody}
        }

        trait Ab extends AB {
          override def a: Int = ${vista.Constants.forbiddenMethodBody}
          def b: Int = 2
        }
      """

    source |> addInsts

    val common = commonMethods(OpVistas("Aa", "Ab", "aa", "ab", "A"))
    common shouldBe empty
  }
}
