package vista.semantics

import org.scalatest._
import scala.meta._
import scala.meta.contrib._

/**
  * @author Paulius Imbrasas
  */
class SClassTest extends WordSpec with Matchers {
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

        val sclass = new SClass(c)
        sclass.methods shouldNot be (Seq.empty)
        sclass.vars should be (Seq.empty)

        val expected = q"def a(): Int = { 5 + 5 }"
        sclass.methods.head.isEqual(expected) should be (true)
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

        val sclass = new SClass(c)

        sclass.methods shouldNot be (Seq.empty)
        val expected = q"def a(p1: Int, p2: Double): Int = { 5 + 5 }"
        sclass.methods.head.isEqual(expected) should be (true)
      }
    }
  }
}
