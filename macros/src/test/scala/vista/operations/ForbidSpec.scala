package vista.operations

import org.scalatest._
import scala.meta._
import scala.meta.contrib._

/**
  * @author paulius
  */
class ForbidSpec extends FlatSpec with Matchers {
  "Forbid definition" should "create the appropriate set of classes" in {
    val expected =
      q"""
        trait Af extends A {
          override def a(): Int = throw new NoSuchMethodException
          override def b(): Int = throw new NoSuchMethodException
        }
        class Afc extends Af {}
        val ab = new Afc()
      """
    val success =
      q"""
        val ab: Af = ∖[A](a, {
          def a(): Int = ???
          def b(): Int = ???
        })
      """

    Forbid(success).isEqual[Structurally](expected) should be (true)
  }
}
