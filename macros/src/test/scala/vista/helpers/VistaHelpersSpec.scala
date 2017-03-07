package vista.helpers

import org.scalatest._
import scala.meta._

/**
  * @author paulius
  */
class VistaHelpersSpec extends FlatSpec with Matchers {
  "A union symbol" should "be detected as a union" in {
    val success = q"∪[A, B](a, b)"
    val fail = q"∖[A, B](a, b)"

    VistaHelpers.isUnion(success) should be (true)
    VistaHelpers.isUnion(fail) should be (false)
  }

  "A difference symbol" should "be detected as forbid" in {
    val success =
      q"""
          {
            val test: Af = ∖[A](a, {
              def a(): Int = ???
            })
          }
        """
    val fail =
      q"""
         {
            ∪[A](a, {})
            ⨯[A, B](a, b)
         }
       """
    VistaHelpers.isForbid(success) should be (true)
    VistaHelpers.isForbid(fail) should be (false)
  }
}
