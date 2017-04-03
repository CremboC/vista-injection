package vista.operations

import vista.FlatSpecBase
import vista.operations.expanders.IntersectOp.Intersect
import vista.operations.parsers.OpVistas
import vista.util.meta.xtensions._

import scala.meta._
import scalaz.Scalaz.ToIdOps

/**
  * @author Paulius Imbrasas
  */
class IntersectTest extends FlatSpecBase {

  behavior of "Intersect"

  private val parseExpandAndRecord = { tree: Term.Apply =>
    val expanded = tree |> parseAndExpand[Term.Apply, OpVistas, Intersect]
    expanded |> addInsts
    expanded
  }

  it should "expand a non-hierarchy" in {
    q"""
          class A {
            def a(): Int = 1
            def b(): Int = 2
            def d(): Int = 4
          }

          class B {
            def a(): Int = 1
            def c(): Int = 3
            def d(): Int = 4
          }
      """ |> addInsts

    val expected =
      q"""
         trait AB extends A with B {
           override def a(): Int = super[A].a()
           override def b() = ${vista.Constants.forbiddenMethodBody}
           override def c() = ${vista.Constants.forbiddenMethodBody}
           override def d(): Int = super[A].d()
         }
      """

    val source = q"""∩[A, B, AB](a, b)"""

    val expanded = parseExpandAndRecord(source)

    expanded.syntax should equal(expected.syntax)
    db("AB").visibilities.signatures should contain only (
      q"def a(): Int = {}".signature,
      q"def d(): Int = {}".signature
    )

    db("AB").forbidden.signatures should contain only (
      q"def b(): Int = {}".signature,
      q"def c(): Int = {}".signature
    )
  }

  it should "expand correctly a hierarchy" in {
    q"""
          class Ap {
            def c(): Int = 3
          }

          class A extends Ap {
            def a(): Int = 1
            def b(): Int = 2
            def d: Int = 4
          }

          class B {
            def a(): Int = 1
            def c(): Int = 3
            def d: Int = 4
          }
      """ |> addInsts

    val expected =
      q"""
         trait AB extends A with B {
           override def a(): Int = super[A].a()
           override def b() = ${vista.Constants.forbiddenMethodBody}
           override def c(): Int = super[A].c()
           override def d: Int = super[A].d
         }
      """

    val source = q"""∩[A, B, AB](a, b)"""

    val expanded = parseExpandAndRecord(source)
    expanded.syntax should equal(expected.syntax)

    db("AB").visibilities.signatures should contain only (
      q"def a(): Int = {}".signature,
      q"def c(): Int = {}".signature,
      q"def d: Int = {}".signature
    )

    db("AB").forbidden.signatures should contain only q"def b(): Int = {}".signature
  }

  it should "expand correctly with common functions" in {
    q"""
          class A {
            def a: Int = 1
            def b(): Int = 2
            def d(): Int = 4
            def common: Int = 0xCCC
          }

          class B {
            def a: Int = 1
            def c(): Int = 3
            def d(): Int = 4
            def common: Int = 0xCCC
          }
      """ |> addInsts

    val expected =
      q"""
         trait AB extends A with B {
           override def a: Int = super[A].a
           override def b() = ${vista.Constants.forbiddenMethodBody}
           override def c() = ${vista.Constants.forbiddenMethodBody}
           override def common: Int = super[A].common
           override def d(): Int = super[A].d()
         }
      """

    val source = q"""∩[A, B, AB](a, b)"""

    val expanded = parseExpandAndRecord(source)
    expanded.syntax should equal(expected.syntax)

    db("AB").visibilities.signatures should contain only (
      q"def a(): Int = {}".signature,
      q"def d(): Int = {}".signature,
      q"def common: Int = {}".signature
    )

    db("AB").forbidden.signatures should contain only (
      q"def b(): Int = {}".signature,
      q"def c(): Int = {}".signature
    )
  }

  it should "expand correctly parametrised classes" in {
    q"""
          class A(b: Int) {
            def a: Int = 1
          }

          class B(n: Int) {
            def a: Int = 1
          }
      """ |> addInsts

    val expected =
      q"""
          trait AB extends A with B {
            override def a: Int = super[A].a
          }
       """

    val source = q"""∩[A, B, AB](a, b)"""

    val expanded = parseExpandAndRecord(source)
    expanded.syntax should equal(expected.syntax)
    db("AB").visibilities.signatures should contain only q"def a(): Int = {}".signature

    db("AB").forbidden shouldBe empty
  }

}
