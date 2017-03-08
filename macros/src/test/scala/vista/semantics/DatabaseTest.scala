package vista.semantics

import org.scalatest._
import vista.operations.Forbid

import scala.meta._

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

        val db = Database
        source.collect {
          case c: Defn.Class => c
        }.foreach(db.addClass)

        db.classes shouldNot be (Set.empty)

        val xClass :: yClass :: Nil = db.classes.toList

        xClass.methods should contain allOf (
          SMethod("one", Seq.empty, "Int"),
          SMethod("two", Seq.empty, "Int")
        )

        yClass.methods should contain allOf (
          SMethod("three", Seq.empty, "Int"),
          SMethod("four", Seq(SParam("param", "Int")), "Int")
        )
      }
    }
  }

}
