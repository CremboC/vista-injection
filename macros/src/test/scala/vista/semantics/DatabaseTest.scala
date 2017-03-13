package vista.semantics

import org.scalatest._

import scala.meta._
import _root_.meta.XMetaIterable
import vista.{ResetsDatabase, defnStructureEquality}

/**
  * @author Paulius Imbrasas
  */
class DatabaseTest extends WordSpec with Matchers with ResetsDatabase {

  "A database" when {
    "parsing multiple classes" should {
      "contain them all" in {
        val source =
          q"""
             class X {
               val n: Int = other
               def one(): Int = other
               def two(): Int = 2
             }

             class Y {
               private val other = 3
               def three(): Int = other
               def four(param: Int): Int = 4
             }
            """

        implicit val db = vista.semantics.Database
        source.collect {
          case c: Defn.Class => db.addClass(c)
        }

        db.classes shouldNot be (Set.empty)

        val xClass = db.get("X")
        val xTests = Set(q"def one(): Int = other", q"def two(): Int = 2").structurally

        xClass.methods.structurally.forall(xTests.contains) should be (true)

        val yClass = db.get("Y")
        val yTests = Set(q"def three(): Int = other", q"def four(param: Int): Int = 4").structurally

        yClass.methods.structurally.forall(yTests.contains) should be (true)
      }
    }

    "parsing hierarchy of classes" should {
      "contain them all" in {
        val source =
          q"""
             class Xp {
               def parent(): Int = 999
             }

             class X extends Xp {
               private val other = 5
               val n: Int = other
               def one(): Int = other
               def two(): Int = 2
             }

             class Y {
               private val other = 3
               def three(): Int = other
               def four(param: Int): Int = 4
             }
            """

        val expected = q"def parent(): Int = 999"

        implicit val db = vista.semantics.Database
        source.collect {
          case c: Defn.Class => db.addClass(c)
        }

        db.get("X").methods should contain (expected)
      }
    }
  }

}
