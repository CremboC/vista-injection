package vista.operations

import org.scalatest._
import vista.helpers.OpHelpers.isForbid
import vista.operations.expanders.ForbidOp
import vista.operations.parsers.OpOverload
import vista.{ResetsDatabase, termBlockStructureEquality, treeStructureEquality}

import scala.meta._
import scala.collection.immutable.Seq

/**
  * @author Paulius Imbrasas
  */
class ForbidTest extends WordSpec with Matchers with ResetsDatabase {
  "Forbid" when {
    "given a val definition" should {
      "create correct classes" in {
        val expected =
          q"""
          trait Af extends A {
            override def a(s: String): Int = throw new NoSuchMethodException
            override def b(): Int = throw new NoSuchMethodException
          }
          val ab = new Af {}
          """
        val source =
          q"""
            val ab: Af = ∖[A](a, {
              def a(s: String): Int = ???
              def b(): Int = ???
            })
          """

        val expanded = parseAndExpand[Defn.Val, OpOverload, ForbidOp.Forbid](source)
        expanded should equal (expected)
      }

      "handle a complex case" in {
        val expected =
          q"""
            trait Bf extends B {
              override def sayHi(a: Int): Unit = throw new NoSuchMethodException
            }
            val b1 = new Bf {}
          """
        val source =
          q"""
           val b1: Bf = ∖[B](b, {
             def sayHi(a: Int): Unit = ???
           })
          """

        val expanded = parseAndExpand[Defn.Val, OpOverload, ForbidOp.Forbid](source)
        expanded should equal (expected)
      }

      "expand in-place without a surrounding block" in {
        val expected =
          q"""
            def test(): A = {
              val s = Seq(1, 2, 3)
              val os = s.map(_ * 2)
              trait Af extends A {
               override def a(s: String): Int = throw new NoSuchMethodException
               override def b(): Int = throw new NoSuchMethodException
              }
              val ab = new Af {}
              ab
            }
           """
        val input =
          q"""
             def test(): A = {
                val s = Seq(1, 2, 3)
                val os = s.map(_ * 2)
                val ab: Af = ∖[A](a, {
                 def a(s: String): Int = ???
                 def b(): Int = ???
                })
                ab
             }
           """

        val result = input.transform {
          case b: Term.Block if isForbid(b) =>
            val modified = b.stats.collect(ForbidModifiers.defnValModifier orElse {
              case o => Term.Block(Seq(o))
            }).flatMap(_.stats)

            Term.Block(modified)
        }
        result should equal (expected)
      }
    }

    "given a def definition" should {
//      "create the correct classes" in {
//        val expected =
//          q"""
//              def test(a: A): Af = {
//                trait Af extends A {
//                  override def imp[A](s: A): A = throw new NoSuchMethodException
//                }
//                new Af {}
//              }
//          """
//        val success =
//          q"""
//             def test(a: A): Af = ∖[A](a, {
//                  def imp[A](s: A): A = ???
//                })
//          """
//
//
//
////        implicit val db = semantics.Database
////        Forbid(success) should equal (expected)
//        ???
//      }

//      "expand in-place with a new block" in {
//        val expected =
//          q"""
//            def test(a: A): Af = {
//              trait Af extends A {
//               override def a(s: String): Int = throw new NoSuchMethodException
//               override def b(): Int = throw new NoSuchMethodException
//              }
//              new Af {}
//            }
//           """
//        val input =
//          q"""
//             def test(a: A): Af = ∖[A](a, {
//                              def a(s: String): Int = ???
//                              def b(): Int = ???
//                            })
//           """
//
//
//        implicit val db = vista.semantics.Database
////        val result = input.transform {
////          case b: Term.Block if isForbid(b) =>
////            val modified = b.stats.collect(Forbid.modifier orElse {
////              case o => Term.Block(Seq(o))
////            }).flatMap(_.stats)
////
////            Term.Block(modified)
////        }
////        result should equal (expected)
//        ???
//      }
    }
  }

}
