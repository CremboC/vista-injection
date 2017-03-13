package vista.operations

import org.scalatest._
import vista.operations.expanders.UnionOp
import vista.operations.parsers.OpVistas
import vista.semantics

import scala.meta._
import vista.termBlockStructureEquality

/**
  * @author Paulius Imbrasas
  */
class UnionizeTest extends FlatSpec with Matchers {
  "Create union" should "create the appropriate set of classes" in {
    val expected =
      q"""
          trait AB extends A with B
          val ab = new AB {}
      """
    val source =
      q"""
         val ab: AB = ∪[A, B](a, b)
        """

    val result = parseAndExpand[Defn.Val, OpVistas, UnionOp.Union](source)
    result should equal (expected)
  }

  "Create union" should "fail when a type is missing" in {
    val source = q"val ab: AB = ∪[A](a, b)"
    an [Exception] should be thrownBy {
      parseAndExpand[Defn.Val, OpVistas, UnionOp.Union](source)
    }
  }

  "Create union" should "fail when the result type is missing" in {
    val source = q"val ab = ∪[A, B](a, b)"
    an [Exception] should be thrownBy {
      parseAndExpand[Defn.Val, OpVistas, UnionOp.Union](source)
    }
  }
}
