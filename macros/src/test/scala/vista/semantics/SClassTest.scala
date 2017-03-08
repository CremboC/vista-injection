package vista.semantics

import org.scalatest._
import scala.meta._

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

        sclass.methods.head should be {
          SMethod("a", Seq.empty, "Int")
        }
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
        sclass.methods.head should be {
          SMethod("a", Seq(SParam("p1", "Int"), SParam("p2", "Double")), "Int")
        }
      }
    }
  }
}
