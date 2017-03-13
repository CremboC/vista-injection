package vista.operations

import org.scalatest._
import vista.operations.expanders.UnionOp.Union
import vista.operations.parsers.OpVistas
import vista.{ResetsDatabase, semantics, termBlockStructureEquality}

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
class UnionTest extends WordSpec with Matchers with ResetsDatabase {

  "A union" when {
    "expanding a simple case" should {
      "expand correctly" in {
        val expected =
            q"""
              trait AB extends A with B {}
              val ab = new AB {}
            """
        val source =
            q"""
              val ab: AB = ∪[A, B](a, b)
            """

        q"""class A; class B""".collect { case c: Defn.Class => semantics.Database.addClass(c) }

        val result = parseAndExpand[Defn.Val, OpVistas, Union](source)
        result should equal(expected)
      }
    }

    "expanding a case with common methods" should {
      "expand correctly" in {
        val classes =
          q"""
              class A {
                def a: Int = 1
                def b: Int = 2
                def o(p: String): String = p
                def g[T]: Int = 5
              }

              class B {
                def a: Int = 1
                def c: Int = 3
                def o(p: String): String = p
                def g[T]: Int = 3
              }
            """
        val source =
          q"""
              val ab: AB = ∪[A, B](a, b)
            """

        val expected =
          q"""
              trait AB extends A with B {
                override def a: Int = super[A].a
                override def g[T]: Int = super[A].g[T]
                override def o(p: String): String = super[A].o(p)
              }
              val ab = new AB {}
            """

        classes.collect { case c: Defn.Class => semantics.Database.addClass(c) }
        val expanded = parseAndExpand[Defn.Val, OpVistas, Union](source)
        expanded.syntax should equal(expected.syntax)
      }
    }

    "expanding a wrong union" should {
      "fail when one of the types is missing" in {
        val source = q"val ab: AB = ∪[A](a, b)"
        an[Exception] should be thrownBy {
          parseAndExpand[Defn.Val, OpVistas, Union](source)
        }
      }

      "fail when the result type is missing" in {
        val source = q"val ab = ∪[A, B](a, b)"
        an[Exception] should be thrownBy {
          parseAndExpand[Defn.Val, OpVistas, Union](source)
        }
      }
    }
  }
}
