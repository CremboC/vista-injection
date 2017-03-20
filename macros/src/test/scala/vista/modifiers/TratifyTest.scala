package vista.modifiers

import org.scalatest._
import scala.meta._

import vista.treeStructureEquality

/**
  * @author Paulius Imbrasas
  */
class TratifyTest extends FlatSpec with Matchers {

  "Tratify" should "convert a parameter-less class into a trait" in {
    val source = q"class A"
    val expected: Tree = q"trait A"

    val result = Tratify(source)
    result should equal(expected)
  }

  "Tratify" should "convert a parameter-less class with a body into a trait" in {
    val source = q"class A { def f: Int = 5 }"
    val expected: Tree = q"trait A { def f: Int = 5 }"

    val result = Tratify(source)
    result should equal(expected)
  }

  "Tratify" should "convert a class with non-specific parameter into a trait" in {
    val source = q"class A(a: String)"
    val expected: Tree =
      q"""
          trait A {
            private val a: String
          }
        """

    val result = Tratify(source)
    result should equal(expected)
  }

  "Tratify" should "convert a class with val param into a trait" in {
    val source = q"class A(val a: String)"
    val expected: Tree =
      q"""
          trait A {
            val a: String
          }
        """

    val result = Tratify(source)
    result should equal(expected)
  }

  "Tratify" should "convert a class with private val param into a trait" in {
    val source = q"class A(private val a: String)"
    val expected: Tree =
      q"""
          trait A {
            private val a: String
          }
        """

    val result = Tratify(source)
    result should equal(expected)
  }

  "Tratify" should "convert a class with var param into a trait" in {
    val source = q"class A(var a: String)"
    val expected: Tree =
      q"""
          trait A {
            var a: String
          }
        """

    val result = Tratify(source)
    result should equal(expected)
  }

  "Tratify" should "convert a class with private var param into a trait" in {
    val source = q"class A(private var a: String)"
    val expected: Tree =
      q"""
          trait A {
            private var a: String
          }
        """

    val result = Tratify(source)
    result should equal(expected)
  }

  "Tratify" should "convert a class with final val param into a trait" in {
    val source = q"class A(final val a: String)"
    val expected: Tree =
      q"""
          trait A {
            val a: String
          }
        """

    val result = Tratify(source)
    result should equal(expected)
  }

  "Tratify" should "convert a class with final var param into a trait" in {
    val source = q"class A(final var a: String)"
    val expected: Tree =
      q"""
          trait A {
            var a: String
          }
        """

    val result = Tratify(source)
    result should equal(expected)
  }

  "Tratify" should "convert a class with mixed params into a trait" in {
    val source = q"class A(a: String, val b: Int, private var c: Int)"
    val expected: Tree =
      q"""
          trait A {
            private val a: String
            val b: Int
            private var c: Int
          }
        """

    val result = Tratify(source)
    result should equal(expected)
  }
}

