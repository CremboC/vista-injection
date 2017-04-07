package vista.operations

import vista.FlatSpecBase
import vista.operations.expanders.ProductOp
import vista.operations.expanders.ProductOp.Product
import vista.operations.parsers.OpVistas
import vista.util.Pipe._

import scala.meta._
import scala.meta.contrib._

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
          val ${ProductOp.LeftName.asPat}: Vista[X]
          val ${ProductOp.RightName.asPat}: Vista[Y]

         @vista.product.pair def f(p1: Int) = new {
            def g(p2: String) = ${ProductOp.LeftName}.f(p1) -> ${ProductOp.RightName}.g(p2)
            def r = ${ProductOp.LeftName}.f(p1) -> ${ProductOp.RightName}.r
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
          val ${ProductOp.LeftName.asPat}: Vista[XY]
          val ${ProductOp.RightName.asPat}: Vista[N]
    
          @vista.product.pair def f(p1: Int) = new {
            def g(p2: String) = new {
              def a(p3: Double) = ${ProductOp.LeftName}.f(p1).g(p2) -> ${ProductOp.RightName}.a(p3)
            }
    
            def r = new {
              def a(p4: Double) = ${ProductOp.LeftName}.f(p1).r -> ${ProductOp.RightName}.a(p4)
            }
          }
        }
      """

    val input = q"x[XY, N, XYN](xy, n)"

    val expanded = parseAndExpand[Term.Apply, OpVistas, Product](input)
    expanded should equal(expected)
  }

  it should "expand an methods with type parameters" in {
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
        trait AB {
          val ${ProductOp.LeftName.asPat}: Vista[A]
          val ${ProductOp.RightName.asPat}: Vista[B]

         @vista.product.pair def a[Tvista1, Tvista2](p1: Tvista1) = new {
           def b[Tvista3](p2: Tvista3) = ${ProductOp.LeftName}.a[Tvista1, Tvista2](p1) -> ${ProductOp.RightName}.b[Tvista3](p2)
         }
        }
      """

    val input = q"x[A, B, AB](a, b)"

    val expanded = parseAndExpand[Term.Apply, OpVistas, Product](input)
    expanded should equal(expected)
  }

  it should "expand methods with multiple parameters" in {
    q"""
        class A {
          def a(v: String, m: String): Int = v.toInt
        }

        class B {
          def b: Int = 3
        }
      """ |> addInsts

    val expected =
      q"""
          trait AB {
            val ${ProductOp.LeftName.asPat}: Vista[A]
            val ${ProductOp.RightName.asPat}: Vista[B]

            @vista.product.pair def a(p1: String, p2: String) = new {
              def b = ${ProductOp.LeftName}.a(p1, p2) -> ${ProductOp.RightName}.b
            }
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
          def b(l: Double): Double = l
        }
    """ |> addInsts

    val f = parseAndExpand[Term.Apply, OpVistas, Product] _

    q"⨯[X, Y, XY](x, y)" |> f |> addInsts
    q"⨯[N, M, NM](n, m)" |> f |> addInsts
    val result = q"⨯[NM, XY, NMXY](nm, xy) " |> f

    val expected = q"""
      trait NMXY {
        val ${ProductOp.LeftName.asPat}: Vista[NM]  
        val ${ProductOp.RightName.asPat}: Vista[XY]

        @vista.product.pair
        def a(p1: Double) = new {
          def b(p2: Double) = new {
            def f[A <: Int](p3: A) = new {
              def g[A <: String](p4: A) = ${ProductOp.LeftName}.a(p1).b(p2) -> ${ProductOp.RightName}.f(p3).g(p4)
            }
          }
        }  
      }
      """

    result should equal(expected)
  }
}
