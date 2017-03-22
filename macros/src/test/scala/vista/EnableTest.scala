package vista

import vista.helpers.OpHelpers.hasOp
import vista.operations.{ForbidModifiers, UnionModifiers}

import scala.collection.immutable.Seq
import scala.meta._

/**
  * @author Paulius Imbrasas
  */
class EnableTest extends WordSpecBase {

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
               trait AB extends A with B {}
               val ab = new AB {}
               ab.nonBDef()
               ab.say("Hello from B")
             }
          """

        q"class A; class B".collect { case c: Defn.Class => semantics.Database.add(c) }

        val modifiers = Seq(
          ForbidModifiers.defnValModifier,
          UnionModifiers.defnValModifier
        ).reduce(_ orElse _)

        val result = source.transform {
          case b: Term.Block if hasOp(b) =>
            val modified = b.stats
              .collect(modifiers orElse {
                case o => Term.Block(Seq(o))
              })
              .flatMap(_.stats)

            Term.Block(modified)
        }
        result should equal(expected)
      }
    }
  }
}
