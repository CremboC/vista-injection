package vista.operations

import vista.FlatSpecBase
import vista.operations.expanders.ProductOp.Product
import vista.operations.parsers.OpVistas
import vista.util.Pipe._

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
class ProductTest extends FlatSpecBase {
  behavior of "Product"

  it should "expand two simple classes" in {
    q"""
         class X {
           def f(i: Int): Int = i * 2
         }

         class Y {
           def g(s: String): Char = s.head
           def r: Boolean = true
         }
      """ |> addInsts

    val expected =
      q"""
        trait XY {
          val left: Vista[X]
          val right: Vista[Y]

          def f(p1: Int) = new {
            def g(p2: String) = left.f(p1) -> right.g(p2)
            def r = left.f(p1) -> right.r
          }
        }
      """

    val input = q"x[X, Y, XY](x, y)"

    val expanded = parseAndExpand[Term.Apply, OpVistas, Product](input)
    expanded should equal(expected)
  }

  it should "expand product of product and normal class" in {
    q"""
        trait XY {
          val left: Vista[X]
          val right: Vista[Y]

          def f(p1: Int) = new {
            def g(p2: String) = left.f(p1) -> right.g(p2)
            def r = left.f(p1) -> right.r
          }
        }

        trait N {
          def a(l: Double): Double = l
        }
    """ |> addInsts

    val expected =
      q"""
        trait XYN {
          val left: Vista[XY]
          val right: Vista[N]
    
          def f(i: Int) = new {
            def g(s: String) = new {
              def a(l: Double) = left.f(i).g(s) -> right.a(l)
            }
    
            def r = new {
              def a(l: Double) = left.f(i).r -> right.a(l)
            }
          }
        }
      """

    val input = q"x[XY, N, XYN](xy, n)"

    val expanded = parseAndExpand[Term.Apply, OpVistas, Product](input)
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
            def ab(p1: String)() = (super[A].a(p1), super[B].b())
          }
        """

    val input = q"x[A, B, AB](a, b)"

    val expanded = parseAndExpand[Term.Apply, OpVistas, Product](input)
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
          def ab[Tvista1, Tvista2, Tvista3](p1: Tvista1)(p2: Tvista3) = (super[A].a[Tvista1, Tvista2](p1), super[B].b[Tvista3](p2))
        }
      """

    val input = q"x[A, B, AB](a, b)"

    val expanded = parseAndExpand[Term.Apply, OpVistas, Product](input)
    expanded should equal(expected)
  }

  it should "expand classes with constructors" in {
    q"""
        class A(p: Int) {
          def a(v: String, m: String): Int = v.toInt
        }

        class B(p1: Int) {
          def b: Int = 3
        }
      """ |> addInsts

    val expected =
      q"""
          trait AB extends A with B {
            def ab(p1: String, p2: String)() = (super[A].a(p1, p2), super[B].b)
          }
        """

    val input = q"x[A, B, AB](a, b)"

    val expanded = parseAndExpand[Term.Apply, OpVistas, Product](input)
    expanded should equal(expected)
  }
}
