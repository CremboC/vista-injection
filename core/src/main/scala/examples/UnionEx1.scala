package examples

import vista.lib._

object Other {
  class F {
    def five: Int = 5
  }
}

@vista.enable
object UnionEx1 extends ExampleBase {
  import Other.F

  class Ap {
    def zero: Int = 0
  }

  class A extends Ap {
    def one(): Int = 1
    def two(): Int = 2
  }

  class B {
    def two(): Int   = 2
    def three(): Int = 3
  }

  def acceptsAB(ab: Vista[AB]): Boolean = ab.isInstanceOf[Vista[AB]]

  def main(args: Array[String]): Unit = {
    val a = new A
    val b = new B

    val ab = ∪[A, B, AB](a, b)

    println(ab.zero)
    println(ab.one())
    println(ab.two())
    println(ab.three())

    println(ab.isInstanceOf[A])
    println(ab.isInstanceOf[B])

    println(ab.isInstanceOf[Vista[AB]])

    val f = new F
    println(f.five)
  }
}
