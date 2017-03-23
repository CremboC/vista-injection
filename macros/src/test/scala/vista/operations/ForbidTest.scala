package vista.operations

import vista.WordSpecBase
import vista.helpers.OpHelpers.isForbid
import vista.meta.xtensions._
import vista.operations.expanders.Expander
import vista.operations.expanders.ForbidOp.Forbid
import vista.operations.parsers.{OpOverload, OpVistas}

import scala.collection.immutable.Seq
import scala.meta._
import scalaz.Scalaz.ToIdOps

/**
  * @author Paulius Imbrasas
  */
class ForbidTest extends WordSpecBase {
  "Forbid" when {
    "given a val definition" should {
      "create correct classes" in {
        val clazz = q"class A"
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

        vista.semantics.Database.add(clazz)

        val expanded = parseAndExpand[Defn.Val, OpOverload, Forbid](source)
        expanded should equal(expected)
      }

      "handle a complex case" in {
        val clazz = q"class B"
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
        vista.semantics.Database.add(clazz)
        val expanded = parseAndExpand[Defn.Val, OpOverload, Forbid](source)
        expanded should equal(expected)
      }

      "expand in-place without a surrounding block" in {
        val clazz = q"class A"
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
        vista.semantics.Database.add(clazz)
        val result = input.transform {
          case b: Term.Block if isForbid(b) =>
            val modified = b.stats
              .collect(ForbidModifiers.valOverloadModifier orElse {
                case o => Term.Block(Seq(o))
              })
              .flatMap(_.stats)

            Term.Block(modified)
        }
        result should equal(expected)
      }

      "expand a vista diff vista operation" in {
        val classes =
          q"class A { def a: Int = 1; def b: Int = 3 }; class B { def b: Int = 2; def g: Double = 2.1 } "
        classes |> addInsts

        val source =
          q"""
            val ab: AB = ∖[A, B](a, b)
          """

        val expected =
          q"""
              trait AB extends A with B {
                override def a: Int = super[A].a
                override def b: Int = throw new NoSuchMethodException
                override def g: Double = throw new NoSuchMethodException
              }
              val ab = new AB {}
          """

        val expanded = source |> parseAndExpand[Defn.Val, OpVistas, Forbid]
        expanded |> addInsts

        expanded.syntax should equal(expected.syntax)

        val visibilities = db("AB").visibilities
        visibilities should not be empty
        visibilities.signatures should contain only {
          q"def a: Int = 1".signature
        }

        val forbidden = db("AB").forbidden
        forbidden should not be empty
        forbidden.signatures should contain only (
          q"def b: Int = {}".signature,
          q"def g: Double = {}".signature
        )
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

    "given variety of classes" should {
      "only have the correct visibilities" in {
        val classes =
          q"""
              class A { def a: Int = 1; def b: Int = 3 }
              class B { def b: Int = 2; def g: Double = 2.1 }
            """
        classes |> addInsts

        val expanded = Expander[OpVistas, Forbid].expand(OpVistas("A", "B", "a", "b", "AB", Some("ab")))
        expanded |> addInsts

        db("AB").visibilities.signatures should contain only (
          q"def a: Int = {}".signature
        )

        db("AB").forbidden.signatures should contain only (
          q"def b:Int = {}".signature,
          q"def g:Double = {}".signature
        )
      }
    }
  }

}
