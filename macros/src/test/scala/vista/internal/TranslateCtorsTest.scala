package vista.internal

import vista.FlatSpecBase

import scala.meta._
import scala.meta.contrib._

class TranslateCtorsTest extends FlatSpecBase {
  behavior of "Translating Constructors"

  it should "translate class without ctor" in {
    val source =
      q"""
        class A
      """

    val (primary, auxs) = TranslateCtors(source)

    primary should equal(q"def $builderName() = new A")
    auxs shouldBe empty
  }

  it should "translate class with a ctor" in {
    val source =
      q"""
        class A(val s: String)
      """

    val (primary, auxs) = TranslateCtors(source)

    primary should equal(q"def $builderName(_s: String) = new A(_s)")
    auxs shouldBe empty
  }

  it should "translate class with ctor and aux ctors" in {
    val source =
      q"""
        class Wibble(foo: Int, bar: String)(f: Int) {
        
          println("Wibble wobble")
        
          def this(t: (Int, String)) = {
            this(t._1, t._2)("hi")
            println("You can execute more code here")
          }
        
          def this(baz: List[Any]) = {
            this(1, "hi")("hi")
            println(this.foo)
            println("You can also execute some code here")
          }
        }
      """

    val (primary, auxs) = TranslateCtors(source)

    primary should equal(
      q"def $builderName(_foo: Int, _bar: String)(_f: Int) = new Wibble(_foo, _bar)(_f)")

    auxs should contain only (
      q"""
         def $builderName(t: (Int, String)): Wibble = {
           val ${selfName.asPat} = $builderName(t._1, t._2)("hi")
           println("You can execute more code here")
           $selfName
         }
      """,
      q"""
         def $builderName(baz: List[Any]): Wibble = {
           val ${selfName.asPat} = $builderName(1, "hi")("hi")
           println(this.foo)
           println("You can also execute some code here")
           $selfName
         }
      """
    )
  }
}
