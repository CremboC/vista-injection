package vista.semantics

import org.scalatest._
import vista.ResetsDatabase

import scala.meta._
import scala.meta.contrib._

/**
  * @author Paulius Imbrasas
  */
class SClassTest extends WordSpec with Matchers with ResetsDatabase {
  "An SClass" when {
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

        implicit val db = vista.semantics.Database
        val sclass = new SClass(c)
        sclass.methods shouldNot be(Seq.empty)
        sclass.vars should be(Seq.empty)

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

        implicit val db = vista.semantics.Database
        source.collect { case c: Defn.Class => db.addClass(c) }

        import meta.XMetaIterable

        db.get("A").methods.mintersect(expected) should have (size (2))
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

        implicit val db = vista.semantics.Database
        source.collect { case c: Defn.Class => db.addClass(c) }

        import meta.XMetaIterable

        db.get("A").methods.mintersect(expected) should have (size (1))
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

        implicit val db = vista.semantics.Database
        val sclass = new SClass(c)

        sclass.methods shouldNot be(Seq.empty)
        val expected = q"def a(p1: Int, p2: Double): Int = { 5 + 5 }"
        sclass.methods.head.isEqual(expected) should be(true)
      }
    }
  }
}