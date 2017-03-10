package vista.operations

import org.scalatest._
import vista.semantics

import scala.meta._
import scala.meta.contrib._

/**
  * @author paulius
  */
class ForbidTest extends WordSpec with Matchers {

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
        val success =
          q"""
            val ab: Af = ∖[A](a, {
              def a(s: String): Int = ???
              def b(): Int = ???
            })
          """

        implicit val db = semantics.Database
        Forbid(success).isEqual[Structurally](expected) should be(true)
      }

      "handle a complex case" in {
        val expected =
          q"""
            trait Bf extends B {
              override def sayHi(a: Int): Unit = throw new NoSuchMethodException
            }
            val b1 = new Bf {}
          """
        val success =
          q"""
           val b1: Bf = ∖[B](b, {
             def sayHi(a: Int): Unit = ???
           })
          """

        implicit val db = semantics.Database
        Forbid(success).isEqual[Structurally](expected) should be(true)
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


        implicit val db = vista.semantics.Database

        val result = input.transform(Forbid.transformer)
        result.isEqual(expected) should be (true)
      }
    }

    "given a def definition" should {
      "create the correct classes" in {
        val expected =
          q"""
              def test(a: A): Af = {
                trait Af extends A {
                  override def imp[A](s: A): A = throw new NoSuchMethodException
                }
                new Af {}
              }
          """
        val success =
          q"""
             def test(a: A): Af = ∖[A](a, {
                  def imp[A](s: A): A = ???
                })
          """

        implicit val db = semantics.Database
        Forbid(success).isEqual[Structurally](expected) should be(true)
      }

      "expand in-place with a new block" in {
        val expected =
          q"""
            def test(a: A): Af = {
              trait Af extends A {
               override def a(s: String): Int = throw new NoSuchMethodException
               override def b(): Int = throw new NoSuchMethodException
              }
              new Af {}
            }
           """
        val input =
          q"""
             def test(a: A): Af = ∖[A](a, {
                              def a(s: String): Int = ???
                              def b(): Int = ???
                            })
           """


        implicit val db = vista.semantics.Database
        val result = input.transform(Forbid.transformer)
        result.isEqual(expected) should be (true)
      }
    }
  }

}