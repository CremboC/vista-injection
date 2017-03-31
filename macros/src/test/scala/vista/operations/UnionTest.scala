package vista.operations

import vista.FlatSpecBase
import vista.operations.expanders.Constructable
import vista.operations.expanders.UnionOp.Union
import vista.operations.parsers.{OpVistas, Parser}

import scala.meta._
import scalaz.Scalaz.ToIdOps

/**
  * @author Paulius Imbrasas
  */
class UnionTest extends FlatSpecBase {

  behavior of "Union"

  it should "expand two simple classes" in {
    val expected = q"""trait AB extends A with B {}"""
    val source   = q"""∪[A, B, AB](a, b)"""

    q"""class A; class B""" |> addInsts

    val result = parseAndExpand[Term.Apply, OpVistas, Union](source)
    val parsed = Parser[Term.Apply, OpVistas].parse(source).get
    Constructable[Union].members(parsed) shouldBe empty
    result should equal(expected)
  }

  it should "expand classes with common methods" in {
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
    val source = q"""∪[A, B, AB](a, b)"""

    val expected =
      q"""
        trait AB extends A with B {
          override def a: Int = super[A].a
          override def b: Int = super[A].b
          override def c: Int = super[B].c
          override def g[T]: Int = super[A].g[T]
          override def o(p: String): String = super[A].o(p)
        }
      """

    classes |> addInsts
    val expanded = parseAndExpand[Term.Apply, OpVistas, Union](source)
    expanded.syntax should equal(expected.syntax)

    val parsed = Parser[Term.Apply, OpVistas].parse(source).get
    Constructable[Union].members(parsed) shouldBe empty
  }

  it should "expand a class with a constructor" in {
    val classes =
      q"""
          class A(val a: Seq[Int]) {
            def a: Int = 1
            def b: Int = 2
          }

          class B(f: () => Unit, var o: Int) {
            def a: Int = 1
            def c: Int = 3
          }
        """
    val source = q"""∪[A, B, AB](a, b)"""

    val expected =
      q"""
          trait AB extends A with B {
            override def a: Int = super[A].a
            override def b: Int = super[A].b
            override def c: Int = super[B].c
          }
        """

    classes |> addInsts
    val expanded = parseAndExpand[Term.Apply, OpVistas, Union](source)
    expanded.syntax should equal(expected.syntax)

    val parsed = Parser[Term.Apply, OpVistas].parse(source).get
    Constructable[Union].members(parsed) should contain only (
      q"override val a: Seq[Int] = a.a",
      q"override val f: () => Unit = b.f",
      q"override var o: Int = b.o"
    )
  }

  it should "expand a class hierarchy" in {
    q"""
        class AB {
          def a: Int = 1
          def b: Int = 2
        }

        trait Aa extends AB {
          override def b: Int = throw new NoSuchMethodException
        }

        trait Ab extends AB {
          override def a: Int = throw new NoSuchMethodException
        }
      """ |> addInsts

    val op       = q"∪[Aa, Ab, A](aa, bb)"
    val expanded = parseAndExpand[Term.Apply, OpVistas, Union](op)

    expanded.syntax should equal {
      q"""
          trait A extends Aa with Ab {
            override def a: Int = super[Aa].a
            override def b: Int = super[Ab].b
          }
      """.syntax
    }
  }
}
