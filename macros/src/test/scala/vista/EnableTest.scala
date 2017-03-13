package vista

import org.scalatest._
import vista.helpers.OpHelpers.hasOp
import vista.operations.{Forbid, Unionize}

import scala.meta._
import scala.collection.immutable.Seq


/**
  * @author Paulius Imbrasas
  */
class EnableTest extends WordSpec with Matchers with ResetsDatabase {
  import vista.treeStructureEquality

  "Enable vistas" when {
    "Given a mix of operations" should {
      "Expand all of them" in {
        val source =
          q"""
            def func(): Unit = {
              val b = new B

              val b1: Bf = ∖[B](b, {
                def multi(a: Int)(b: Int): Int = ???
              })

              println(b1.isInstanceOf[B])
              println(acceptsB(b1))

              val a = new A
              val ab: AB = ∪[A, B](a, b)
              ab.nonBDef()
              ab.say("Hello from B")
            }
          """

        val expected =
          q"""
             def func(): Unit = {
               val b = new B

               trait Bf extends B {
                 override def multi(a: Int)(b: Int): Int = throw new NoSuchMethodException
               }
               val b1 = new Bf {}

               println(b1.isInstanceOf[B])
               println(acceptsB(b1))

               val a = new A
               trait AB extends A with B
               val ab = new AB {}
               ab.nonBDef()
               ab.say("Hello from B")
             }
          """

        implicit val db = semantics.Database

        val ops = Seq(Forbid, Unionize)
        val modifiers = ops.map(_.modifier).reduce(_ orElse _)

        val result = source.transform {
          case b: Term.Block if hasOp(b) =>
            val modified = b.stats.collect(modifiers orElse {
              case o => Term.Block(Seq(o))
            }).flatMap(_.stats)

            Term.Block(modified)
        }
        result should equal (expected)
      }
    }
  }
}