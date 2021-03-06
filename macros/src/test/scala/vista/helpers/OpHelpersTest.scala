package vista.helpers

import vista.WordSpecBase

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
class OpHelpersTest extends WordSpecBase {

  "A union symbol" when {
    "written as ∪" should {
      "be detected correctly" in {
        val success = q"∪[A, B](a, b)"
        OpHelpers.isUnion(success) should be(true)
      }
    }

    "written incorrectly" should {
      "not be detected" in {
        val fail = q"u[A, B](a, b)"
        OpHelpers.isUnion(fail) should be(false)
      }
    }
  }

  "A difference symbol" when {
    "written as ∖" should {
      "be detected correctly" in {
        val success =
          q"""
            ∖[A](a, {
              def a(): Int = ???
            })
        """
        OpHelpers.isForbid(success) should be(true)
      }
    }

    "written incorrectly" should {
      "not be detected" in {
        val fail =
          q"""
             {
                ∪[A](a, {})
                ⨯[A, B](a, b)
             }
           """
        OpHelpers.isForbid(fail) should be(false)
      }
    }
  }

  "An intersection symbol" when {
    "written as ∩" should {
      "be detected" in {
        val success = q"∩[A, B](a, b)"
        OpHelpers.isIntersect(success) should be(true)
      }
    }

    "written incorrectly" should {
      "not be detected" in {
        val fail = q"∖[A, B](a, b)"
        OpHelpers.isIntersect(fail) should be(false)
      }
    }
  }

  "A product symbol" when {
    "written as ⨯" should {
      "be detected correctly" in {
        val success = q"⨯[A, B](a, b)"
        OpHelpers.isProduct(success) should be(true)
      }
    }

    "written as x" should {
      "fail" in {
        val fail = q"x[A, B](a, b)"
        OpHelpers.isProduct(fail) should be(false)
      }
    }
  }

  "HasOp" when {
    "used as an extractor" should {
      "succeed" in {
        val apply1 = q"⨯[A, B](a, b)"
        apply1 match {
          case OpHelpers.HasOp(i) => i
        }
      }

      "fail" in {
        val apply1 = q"f[A, B](a, b)"
        assertThrows[MatchError] {
          apply1 match {
            case OpHelpers.HasOp(i) => i
          }
        }
      }
    }
  }
}
