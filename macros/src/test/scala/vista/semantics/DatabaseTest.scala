package vista.semantics

import org.scalatest._

import scala.meta._
import scala.meta.contrib._

/**
  * Created by Crembo on 2017-03-08.
  */
class DatabaseTest extends WordSpec with Matchers {

  "A database" when {
    "parsing multiple classes" should {
      "contain them all" in {
        val source =
          q"""
             class X {
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

        val db = vista.semantics.Database
        source.collect {
          case c: Defn.Class => db.addClass(c)
        }

        db.classes shouldNot be (Set.empty)

        val xClass = db.classes.find(_.name == "X").get
        val xTests = Seq(q"def one(): Int = other", q"def two(): Int = 2")
        val xPairs = xClass.methods.zip(xTests)

        xPairs.foreach { case (l, r) => l.isEqual(r) }

        val yClass = db.classes.find(_.name == "Y").get
        val yTests = Seq(q"def three(): Int = other", q"def four(param: Int): Int = 4")
        val yPairs = yClass.methods.zip(yTests)

        yPairs.foreach { case (l, r) => l.isEqual(r) }
      }
    }
  }

}
