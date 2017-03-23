package vista.operations

import vista.FlatSpecBase
import vista.operations.expanders.ProductOp.Product
import vista.operations.parsers.OpVistas

import scala.meta._
import scalaz.Scalaz.ToIdOps

/**
  * @author Paulius Imbrasas
  */
class ProductTest extends FlatSpecBase {
  behavior of "Product"

  it should "expand two simple classes" in {
    q"""
        class A {
          def a(): Int = 5
        }

        class B {
          def b(): Int = 3
        }
      """ |> addInsts

    val expected =
      q"""
        trait AB extends A with B {
          def ab()() = (a(), b())
        }
        val ab = new AB {}
      """

    val input = q"val ab: AB = x[A, B](a, b)"

    val expanded = parseAndExpand[Defn.Val, OpVistas, Product](input)
    expanded should equal(expected)
  }

  it should "expand an example with parametrised methods" in {
    q"""
        class A {
          def a(v: String): Int = v.toInt
        }

        class B {
          def b(): Int = 3
        }
      """ |> addInsts

    val expected =
      q"""
          trait AB extends A with B {
            def ab(v: String)() = (a(v), b())
          }
          val ab = new AB {}
        """

    val input = q"val ab: AB = x[A, B](a, b)"

    val expanded = parseAndExpand[Defn.Val, OpVistas, Product](input)
    expanded should equal(expected)
  }

  it should "expand an example with type parameters" in {
    q"""
      class A {
        def a[A, B](v: A): B = v.asInstanceOf[B]
      }

      class B {
        def b[A](s: A): Int = s.toInt
      }
    """ |> addInsts

    val expected =
      q"""
        trait AB extends A with B {
          def ab[Tvista1, Tvista2, Tvista3](v: Tvista1)(s: Tvista3) = (a[Tvista1, Tvista2](v), b[Tvista3](s))
        }
        val ab = new AB {}
      """

    val input = q"val ab: AB = x[A, B](a, b)"

    val expanded = parseAndExpand[Defn.Val, OpVistas, Product](input)
    expanded should equal(expected)
  }

  it should "expand classes with constructors" in {
    q"""
        class A(p: Int) {
          def a(v: String): Int = v.toInt
        }

        class B(p1: Int) {
          def b(): Int = 3
        }
      """ |> addInsts

    val expected =
      q"""
          trait AB extends A with B {
            def ab(v: String)() = (a(v), b())
          }
          val ab = new AB {
            override val p: Int = a.p
            override val p1: Int = b.p1
          }
        """

    val input = q"val ab: AB = x[A, B](a, b)"

    val expanded = parseAndExpand[Defn.Val, OpVistas, Product](input)
    expanded should equal(expected)
  }
}
