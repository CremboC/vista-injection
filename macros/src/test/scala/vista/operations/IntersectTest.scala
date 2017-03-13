package vista.operations

import org.scalatest._
import vista.operations.expanders.IntersectOp.Intersect
import vista.operations.parsers.OpVistas

import scala.meta._
import vista.{ResetsDatabase, semantics, termBlockStructureEquality, treeStructureEquality}

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
               override def a(): Int = super[A].a()
               override def b(): Int = throw new NoSuchMethodException
               override def c(): Int = throw new NoSuchMethodException
               override def d(): Int = super[A].d()
             }
             val ab = new AB {}
          """

        val source =
          q"""
            val ab: AB = ∩[A, B](a, b)
          """

        val db = semantics.Database
        classes.collect { case c: Defn.Class => db.addClass(c) }
        val expanded = parseAndExpand[Defn.Val, OpVistas, Intersect](source)

        expanded.syntax should equal(expected.syntax)
      }

      "expand correctly with hierarchy" in {
        val classes =
          q"""
              class Ap {
                def c(): Int = 3
              }

              class A extends Ap {
                def a(): Int = 1
                def b(): Int = 2
                def d: Int = 4
              }

              class B {
                def a(): Int = 1
                def c(): Int = 3
                def d: Int = 4
              }
          """

        val expected =
          q"""
             trait AB extends A with B {
               override def a(): Int = super[A].a()
               override def b(): Int = throw new NoSuchMethodException
               override def c(): Int = super[A].c()
               override def d: Int = super[A].d
             }
             val ab = new AB {}
          """

        val source =
          q"""
            val ab: AB = ∩[A, B](a, b)
          """

        val db = semantics.Database
        classes.collect { case c: Defn.Class => db.addClass(c) }

        val expanded = parseAndExpand[Defn.Val, OpVistas, Intersect](source)
        expanded.syntax should equal(expected.syntax)
      }

      "expand correctly with common functions" in {
        val classes =
          q"""
              class A {
                def a: Int = 1
                def b(): Int = 2
                def d(): Int = 4
                def common: Int = 0xCCC
              }

              class B {
                def a: Int = 1
                def c(): Int = 3
                def d(): Int = 4
                def common: Int = 0xCCC
              }
          """

        val expected =
          q"""
             trait AB extends A with B {
               override def a: Int = super[A].a
               override def b(): Int = throw new NoSuchMethodException
               override def c(): Int = throw new NoSuchMethodException
               override def common: Int = super[A].common
               override def d(): Int = super[A].d()
             }
             val ab = new AB {}
          """

        val source =
          q"""
            val ab: AB = ∩[A, B](a, b)
          """

        val db = semantics.Database
        classes.collect { case c: Defn.Class => db.addClass(c) }
        val expanded = parseAndExpand[Defn.Val, OpVistas, Intersect](source)
        expanded.syntax should equal(expected.syntax)
      }
    }
  }
}
