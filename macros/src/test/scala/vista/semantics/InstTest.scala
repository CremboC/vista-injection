package vista.semantics

import vista.WordSpecBase
import vista.util.Pipe._
import vista.util.meta.xtensions.XSet

import scala.meta._
import scala.meta.contrib._

/**
  * @author Paulius Imbrasas
  */
class InstTest extends WordSpecBase {
  "An Inst.Class" when {
    "given a valid class" should {
      "store it appropriately" in {
        val c =
          q"""
            class A {
              def a(): Int = {
                5 + 5
              }
            }
          """
        val sclass = Inst.Class(c)
        sclass.methods shouldNot be(Seq.empty)

        val expected = q"def a(): Int = { 5 + 5 }"
        sclass.methods.head.isEqual(expected) should be(true)
      }
    }

    "given a class hierarchy" should {
      "present the methods correctly" in {
        val source =
          q"""
              class Ap {
                def b(): Int = 5
              }

              class A extends Ap {
                def a(): Int = {
                  5 + 5
                }
              }
          """

        val expected = Set(q"def b(): Int = 5", q"def a(): Int = { 5 + 5 }")
        source |> addInsts
        db.get("A").methods.mintersect(expected) should have(size(2))
      }

      "present the methods correctly 2" in {
        val source =
          q"""
              class Ap {
                def b(): Int = 5
              }

              class A {
                def a(): Int = {
                  5 + 5
                }
              }
          """

        val expected = Set(q"def a(): Int = { 5 + 5 }")
        source |> addInsts
        db.get("A").methods.mintersect(expected) should have(size(1))
      }
    }

    "given a class with definition containing params" should {
      "store it appropriately" in {
        val c =
          q"""
          class A {
            def a(p1: Int, p2: Double): Int = {
              5 + 5
            }
          }
        """

        val sclass = Inst.Class(c)

        sclass.methods shouldNot be(Seq.empty)
        val expected = q"def a(p1: Int, p2: Double): Int = { 5 + 5 }"
        sclass.methods.head.isEqual(expected) should be(true)
      }
    }
  }
}
