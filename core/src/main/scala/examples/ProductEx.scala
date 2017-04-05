package examples

import vista.lib._

@vista.enable
object ProductEx {
  class Ap {
    def zero: Int = 0
  }

  class A extends Ap {
    def one(): Int = 1
    def two(): Int = 2
  }

  class B(val s: String) {
    def two(): Int   = 2
    def three(): Int = 3
  }

  def mkProduct(a: A, b: B) = ⨯[A, B, mkAxB](a, b)

  def main(args: Array[String]): Unit = {
    val a = new A
    val b = new B("hi")

    val ab = ⨯[A, B, AxB](a, b)

    println(ab.onetwo()())
//    ab.<one, two>

    println(b.s)

    val mkAxB = mkProduct(a, b)
  }
}
