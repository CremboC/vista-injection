package vista.operations

import org.scalatest._
import vista.semantics

import scala.meta._
import scala.meta.contrib._

class UnionizeTest extends FlatSpec with Matchers {
  "Create union" should "create the appropriate set of classes" in {
    val expected =
      q"""
          trait AB extends A with B
          val ab = new AB {}
      """
    val success = q"val ab: AB = ∪[A, B](a, b)"
    implicit val db = semantics.Database
    Unionize(success).isEqual[Structurally](expected) should be (true)
  }

  "Create union" should "fail when a type is missing" in {
    val fail = q"val ab: AB = ∪[A](a, b)"
    implicit val db = semantics.Database
    a [Exception] should be thrownBy {
      Unionize(fail)
    }
  }

  "Create union" should "fail when the result type is missing" in {
    val fail = q"val ab = ∪[A, B](a, b)"
    implicit val db = semantics.Database
    a [Exception] should be thrownBy {
      Unionize(fail)
    }
  }
}
