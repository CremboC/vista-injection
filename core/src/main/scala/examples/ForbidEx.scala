package examples

import vistas.Vista._

/**
  * @author paulius
  */
@vista.enable
object ForbidEx {
  class A {
    def sum(items: Seq[Int]): Int = items.sum
    def convert[A, B <: A](a: A): B = {
      a.asInstanceOf[B]
    }
  }

  def main(args: Array[String]): Unit = {
    val a = new A
    println(a.sum(Seq(1, 2, 3))) // should work

    // forbid the sum method
    val aWithoutSum: AnoSum = ∖[A](a, {
      def sum(items: Seq[Int]): Int = ???
    })

    // make dummy classes for convert method
    class T
    class Ta extends T
    println(aWithoutSum.convert[T, Ta](new T)) // run convert, should work

    aWithoutSum.sum(Seq(1, 2, 3)) // should throw a NoSuchMethodException
  }
}
