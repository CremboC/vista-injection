package vista.operations

import vista.operations.expanders.IntersectOp.Intersect
import vista.operations.parsers.OpVistas
import vista.{WordSpecBase, semantics}

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
class IntersectTest extends WordSpecBase {

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
               override def b() = throw new NoSuchMethodException
               override def c() = throw new NoSuchMethodException
               override def d(): Int = super[A].d()
             }
             val ab = new AB {}
          """

        val source =
          q"""
            val ab: AB = ∩[A, B](a, b)
          """

        val db = semantics.Database
        classes.traverse { case c: Defn.Class => db.add(c) }
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
               override def b() = throw new NoSuchMethodException
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
        classes.traverse { case c: Defn.Class => db.add(c) }

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
               override def b() = throw new NoSuchMethodException
               override def c() = throw new NoSuchMethodException
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
        classes.traverse { case c: Defn.Class => db.add(c) }
        val expanded = parseAndExpand[Defn.Val, OpVistas, Intersect](source)
        expanded.syntax should equal(expected.syntax)
      }

      "expand correctly with parametrised classes" in {
        val classes =
          q"""
              class A(b: Int) {
                def a: Int = 1
              }

              class B(n: Int) {
                def a: Int = 1
              }
          """

        val expected =
          q"""
              trait AB extends A with B {
                override def a: Int = super[A].a
              }
              val ab = new AB {
                override val b: Int = a.b
                override val n: Int = b.n
              }
           """

        val db = semantics.Database
        classes.traverse { case c: Defn.Class => db.add(c) }

        val source =
          q"""
            val ab: AB = ∩[A, B](a, b)
          """

        val expanded = parseAndExpand[Defn.Val, OpVistas, Intersect](source)
        expanded.syntax should equal(expected.syntax)
      }
    }
  }
}
