package vista.modifiers

import vista.{FlatSpecBase, treeStructureEquality}

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
class TratifyTest extends FlatSpecBase {

  "Tratify" should "convert a parameter-less class into a trait" in {
    val source         = q"class A"
    val expected: Tree = q"trait A"

    val result: Tree = Tratify(source)
    result should equal(expected)
  }

  "Tratify" should "convert a parameter-less class with a body into a trait" in {
    val source         = q"class A { def f: Int = 5 }"
    val expected: Tree = q"trait A { def f: Int = 5 }"

    val result: Tree = Tratify(source)
    result should equal(expected)
  }

  "Tratify" should "convert a class with non-specific parameter into a trait" in {
    val source = q"class A(a: String)"
    val expected: Tree =
      q"""
          trait A {
            val a: String
          }
        """

    val result: Tree = Tratify(source)
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

    val result: Tree = Tratify(source)
    result should equal(expected)
  }

  "Tratify" should "convert a class with private val param into a trait" in {
    val source = q"class A(private val a: String)"
    val expected: Tree =
      q"""
          trait A {
            val a: String
          }
        """

    val result: Tree = Tratify(source)
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

    val result: Tree = Tratify(source)
    result should equal(expected)
  }

  "Tratify" should "convert a class with private var param into a trait" in {
    val source = q"class A(private var a: String)"
    val expected: Tree =
      q"""
          trait A {
            var a: String
          }
        """

    val result: Tree = Tratify(source)
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

    val result: Tree = Tratify(source)
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

    val result: Tree = Tratify(source)
    result should equal(expected)
  }

  "Tratify" should "convert a class with mixed params into a trait" in {
    val source = q"class A(a: String, val b: Int, private var c: Int)"
    val expected: Tree =
      q"""
          trait A {
            val a: String
            val b: Int
            var c: Int
          }
        """

    val result: Tree = Tratify(source)
    result should equal(expected)
  }

  "Tratify" should "convert a class constructor into a trait constructor" in {
    val source = q"class A(val a: String)"
    val expected: Tree =
      q"""
          new A {
            override val a: String = "hello"
          }
        """


    val term = q"""new A("hello")"""

    val db = vista.semantics.Database
    db.add(source)

    val result: Tree = Tratify(term)
    result should equal(expected)
  }

  "Tratify" should "convert a complex class constructor into a trait constructor" in {
    val source = q"class A(val a: String, private var b: Int)"
    val expected: Tree =
      q"""
          new A {
            override val a: String = "hello"
            override var b: Int = 5
          }
        """
    
    val term = q"""new A("hello", 5)"""

    val db = vista.semantics.Database
    db.add(source)

    val result: Tree = Tratify(term)
    result should equal(expected)
  }

  "Tratify" should "expand a term extending another class correctly" in {
    val source = q"class A(val a: String)"
    val expected: Tree =
      q"""
          new A with vistas.AnyV {
            override val a: String = "hello"
          }
        """

    val term = q"""new A("hello") with vistas.AnyV"""

    val db = vista.semantics.Database
    db.add(source)

    val result: Tree = Tratify(term)
    result should equal(expected)
  }
}
