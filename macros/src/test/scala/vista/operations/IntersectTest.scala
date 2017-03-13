package vista.operations

import org.scalatest._
import vista.operations.expanders.IntersectOp
import vista.operations.parsers.OpVistas

import scala.meta._
import vista.{ResetsDatabase, semantics, termBlockStructureEquality}

/**
  * @author Paulius Imbrasas
  */
class IntersectTest extends WordSpec with Matchers with ResetsDatabase {

  "Intersect" when {
    "given a val definition" should {
      "expand correctly without hierarchy" in {
        val classes =
          q"""
              class A {
                def a(): Int = 1
                def b(): Int = 2
                def d(): Int = 4
              }

              class B {
                def a(): Int = 1
                def c(): Int = 3
                def d(): Int = 4
              }
          """

        val expected =
          q"""
             trait AB extends A with B {
               override def b(): Int = throw new NoSuchMethodException
               override def c(): Int = throw new NoSuchMethodException
             }
             val ab = new AB {}
          """

        val source =
          q"""
            val ab: AB = ∩[A, B](a, b)
          """

        implicit val db = semantics.Database
        classes.collect { case c: Defn.Class => db.addClass(c) }

        val parsed = parse[Defn.Val, OpVistas](source) getOrElse {
          throw new IllegalArgumentException
        }
        val expanded = expand[OpVistas, IntersectOp.Intersect](parsed)
        expanded should equal (expected)
      }

      "expend correctly with hierarchy" in {
        val classes =
          q"""
              class Ap {
                def c(): Int = 3
              }

              class A extends Ap {
                def a(): Int = 1
                def b(): Int = 2
                def d(): Int = 4
              }

              class B {
                def a(): Int = 1
                def c(): Int = 3
                def d(): Int = 4
              }
          """

        val expected =
          q"""
             trait AB extends A with B {
               override def b(): Int = throw new NoSuchMethodException
             }
             val ab = new AB {}
          """

        val source =
          q"""
            val ab: AB = ∩[A, B](a, b)
          """

        val db = semantics.Database
        classes.collect { case c: Defn.Class => db.addClass(c) }

        val result = parseAndExpand[Defn.Val, OpVistas, IntersectOp.Intersect](source)
        result should equal (expected)
      }
    }
  }
}
