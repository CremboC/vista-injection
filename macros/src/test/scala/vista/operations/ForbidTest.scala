package vista.operations

import vista.WordSpecBase
import vista.meta.xtensions._
import vista.operations.expanders.Expander
import vista.operations.expanders.ForbidOp.Forbid
import vista.operations.parsers.{OpOverload, OpVistas}

import scala.meta._
import scalaz.Scalaz.ToIdOps

/**
  * @author Paulius Imbrasas
  */
class ForbidTest extends WordSpecBase {
  "Forbid" when {
    "given a val definition" should {
      "create correct classes" in {
        val clazz = q"class A"
        val expected =
          q"""
          trait Af extends A {
            override def a(s: String): Int = throw new NoSuchMethodException
            override def b(): Int = throw new NoSuchMethodException
          }
          """
        val source =
          q"""
            ∖[A ~> Af](a, {
              def a(s: String): Int = ???
              def b(): Int = ???
            })
          """

        vista.semantics.Database.add(clazz)

        val expanded = parseAndExpand[Term.Apply, OpOverload, Forbid](source)
        expanded should equal(expected)
      }

      "handle a complex case" in {
        val clazz = q"class B"
        val expected =
          q"""
            trait Bf extends B {
              override def sayHi(a: Int): Unit = throw new NoSuchMethodException
            }
          """
        val source =
          q"""
          ∖[B ~> Bf](b, {
            def sayHi(a: Int): Unit = ???
          })
          """
        vista.semantics.Database.add(clazz)
        val expanded = parseAndExpand[Term.Apply, OpOverload, Forbid](source)
        expanded should equal(expected)
      }

      "expand a vista diff vista operation" in {
        val classes =
          q"class A { def a: Int = 1; def b: Int = 3 }; class B { def b: Int = 2; def g: Double = 2.1 } "
        classes |> addInsts

        val source = q"""∖[A & B ~> AB](a, b)"""

        val expected =
          q"""
              trait AB extends A with B {
                override def a: Int = super[A].a
                override def b: Int = throw new NoSuchMethodException
                override def g: Double = throw new NoSuchMethodException
              }
          """

        val expanded = source |> parseAndExpand[Term.Apply, OpVistas, Forbid]
        expanded |> addInsts

        expanded.syntax should equal(expected.syntax)

        val visibilities = db("AB").visibilities
        visibilities should not be empty
        visibilities.signatures should contain only {
          q"def a: Int = 1".signature
        }

        val forbidden = db("AB").forbidden
        forbidden should not be empty
        forbidden.signatures should contain only (
          q"def b: Int = {}".signature,
          q"def g: Double = {}".signature
        )
      }
    }

    "given variety of classes" should {
      "only have the correct visibilities" in {
        val classes =
          q"""
              class A { def a: Int = 1; def b: Int = 3 }
              class B { def b: Int = 2; def g: Double = 2.1 }
            """
        classes |> addInsts

        val expanded =
          Expander[OpVistas, Forbid].expand(OpVistas("A", "B", "a", "b", "AB", Some("ab")))
        expanded |> addInsts

        db("AB").visibilities.signatures should contain only q"def a: Int = {}".signature

        db("AB").forbidden.signatures should contain only (
          q"def b:Int = {}".signature,
          q"def g:Double = {}".signature
        )
      }
    }
  }

}
