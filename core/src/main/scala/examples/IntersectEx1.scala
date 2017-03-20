package examples

import vistas.Vista.∩

@vista.enable
object IntersectEx1 extends ExampleBase {
  class Ap {
    def zero: Int = -0
  }

  class A extends Ap {
    def one(): Int = 1
    def two(): Double = 2.10
  }

  class B {
    def zero: Int = 0
    def two(): Double = 2.11
    def three(): Int = 3
  }

  def main(args: Array[String]): Unit = {
    val a = new A
    val b = new B

    val ab: AuB = ∩[A, B](a, b)

    try println(ab.zero) catch {
      case e: NoSuchMethodException => println(e.getClass)
    }

    try println(ab.one()) catch {
      case e: NoSuchMethodException => println(e.getClass)
    }

    try println(ab.two()) catch {
      case e: NoSuchMethodException => println(e.getClass)
    }

    try println(ab.three()) catch {
      case e: NoSuchMethodException => println(e.getClass)
    }

  }
}
