package examples

import vistas.Vista.∪

/**
  * @author paulius
  */
@vista.enable
object UnionEx1 extends ExampleBase {
  class Ap {
    def zero: Int = 0
  }

  class A extends Ap {
    def one(): Int = 1
    def two(): Int = 2
  }

  class B {
    def two(): Int = 2
    def three(): Int = 3
  }

  def main(args: Array[String]): Unit = {
    val a = new A
    val b = new B

    val ab: AB = ∪[A, B](a, b)

    println(ab.zero)
    println(ab.one())
    println(ab.two())
    println(ab.three())

    println(ab.isInstanceOf[A])
    println(ab.isInstanceOf[B])
    println(ab.isInstanceOf[AB])
  }
}
