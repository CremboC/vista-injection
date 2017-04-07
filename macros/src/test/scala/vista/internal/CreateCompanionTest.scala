package vista.internal

import vista.FlatSpecBase

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.contrib._

class CreateCompanionTest extends FlatSpecBase {
  behavior of "Create Companion"

  it should "create the companion of class without ctor" in {
    val clazz   = q"class A"
    val primary = q"def $builderName = new A"
    val aux     = Nil

    val result = CreateCompanion(clazz, primary, aux)
    val expected =
      q"""
        object A {
          def $builderName = new A
        }
      """

    result should equal(expected)
  }

  it should "create the companion of class with ctor" in {
    val clazz   = q"class A(s: String)"
    val primary = q"def $builderName(_s: String) = new A(_s)"
    val aux     = Nil

    val result = CreateCompanion(clazz, primary, aux)
    val expected =
      q"""
        object A {
          def $builderName(_s: String) = new A(_s)
        }
      """

    result should equal(expected)
  }

  it should "create the companion of class with ctor and aux ctors" in {
    val clazz   = q"class A(s: String) { def this(s: Int) = this(s.toString) }"
    val primary = q"def $builderName(_s: String) = new A(_s)"
    val aux =
      Seq(q"def $builderName(s: Int) = { val ${selfName.asPat} = $builderName(s); $selfName }")

    val result = CreateCompanion(clazz, primary, aux)
    val expected =
      q"""
        object A {
          def $builderName(_s: String) = new A(_s)
          def $builderName(s: Int) = {
            val ${selfName.asPat} = $builderName(s)
            $selfName
          }
        }
      """

    result should equal(expected)
  }

}
