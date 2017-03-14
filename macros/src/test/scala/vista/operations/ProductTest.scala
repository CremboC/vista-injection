package vista.operations

import org.scalatest._

import scala.meta._
import vista.ResetsDatabase
import vista.operations.parsers.OpVistas
import vista.operations.expanders.ProductOp.Product
import vista.termBlockStructureEquality

/**
  * @author Paulius Imbrasas
  */
class ProductTest extends WordSpec with Matchers with ResetsDatabase {

  "Product" when {
    "given two simple class" should {
      "expand correctly" in {
        val source =
          q"""
              class A {
                def a(): Int = 5
              }

              class B {
                def b(): Int = 3
              }
            """
        val expected =
          q"""
              trait AB extends A with B {
                def ab()() = (a(), b())
              }
              val ab = new AB {}
            """

        val input = q"val ab: AB = x[A, B](a, b)"

        val db = vista.semantics.Database
        source.collect { case c: Defn.Class => db.addClass(c) }

        val expanded = parseAndExpand[Defn.Val, OpVistas, Product](input)
        expanded should equal (expected)
      }
    }

    "given an example with parameters" should {
      "expand correctly" in {
        val source =
          q"""
              class A {
                def a(v: String): Int = v.toInt
              }

              class B {
                def b(): Int = 3
              }
            """
        val expected =
          q"""
              trait AB extends A with B {
                def ab(v: String)() = (a(v), b())
              }
              val ab = new AB {}
            """

        val input = q"val ab: AB = x[A, B](a, b)"

        val db = vista.semantics.Database
        source.collect { case c: Defn.Class => db.addClass(c) }

        val expanded = parseAndExpand[Defn.Val, OpVistas, Product](input)
        expanded should equal (expected)
      }
    }

    "given an example with type parameters" should {
      "expand correctly" in {
        val source =
          q"""
              class A {
                def a[A, B](v: A): B = v.asInstanceOf[B]
              }

              class B {
                def b[A](s: A): Int = s.toInt
              }
            """
        val expected =
          q"""
              trait AB extends A with B {
                def ab[Tvista1, Tvista2, Tvista3](v: Tvista1)(s: Tvista3) = (a[Tvista1, Tvista2](v), b[Tvista3](s))
              }
              val ab = new AB {}
            """

        val input = q"val ab: AB = x[A, B](a, b)"

        val db = vista.semantics.Database
        source.collect { case c: Defn.Class => db.addClass(c) }

        val expanded = parseAndExpand[Defn.Val, OpVistas, Product](input)
        expanded should equal (expected)
      }
    }
  }
}