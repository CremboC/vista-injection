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
        class A {
          def a(): Int = 5
        }

        class B {
          def b: Int = 3
        }
      """ |> addInsts

    val expected =
      q"""
        @vista.product
        trait AB extends A with B {
          def ab()() = (super[A].a(), super[B].b)
        }
      """

    val input = q"x[A, B, AB](a, b)"

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
          @vista.product
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
        def b[A <: String](s: A): Int = s.toInt
      }
    """ |> addInsts

    val expected =
      q"""
        @vista.product
        trait AB extends A with B {
          def ab[Tvista1, Tvista2, Tvista3 <: String](p1: Tvista1)(p2: Tvista3) = (super[A].a(p1), super[B].b(p2))
        }
      """

    val input = q"x[A, B, AB](a, b)"

    val expanded = parseAndExpand[Term.Apply, OpVistas, Product](input)
    expanded should equal(expected)
  }

  it should "expand a product of two product results" in {
    q"""
        class X {
          def f[A <: Int](i: A): Int = i * 2
        }
       
        class Y {
          def g[A <: String](s: A): Char = s.head
        }

        class N {
          def a(l: Double): Double = l
        }

        class M {
          def b: Double = 1.0
        }
    """ |> addInsts

    val f = parseAndExpand[Term.Apply, OpVistas, Product] _

    q"⨯[X, Y, XY](x, y)" |> f |> addInsts // fg
    q"⨯[N, M, NM](n, m)" |> f |> addInsts // ab

    val result = q"⨯[XY, NM, XYNM](xy, nm) " |> f

    val expected =
      q"""
          @vista.product
          trait XYNM extends XY with NM {
            def fgab[Tvista1 <: Int, Tvista2 <: String](p1: Tvista1)(p2: Tvista2)(p3: Double)() = (super[XY].fg(p1)(p2), super[NM].ab(p3)())
          }
      """

    result should equal(expected)
  }

  it should "expand multiple-parameter-list methods" in {
    q"""
        class A {
          def a(v: String)(m: Double): Int = v.toInt
        }

        class B {
          def b(): Int = 3
          def m: Int = 5
        }
      """ |> addInsts

    val expected =
      q"""
          @vista.product
          trait AB extends A with B {
            def ab(p1: String)(p2: Double)() = (super[A].a(p1)(p2), super[B].b())
            def am(p1: String)(p2: Double)() = (super[A].a(p1)(p2), super[B].m)
          }
        """

    val input = q"x[A, B, AB](a, b)"

    val expanded = parseAndExpand[Term.Apply, OpVistas, Product](input)
    expanded should equal(expected)
  }
}
