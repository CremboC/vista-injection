package examples

import vista.lib._

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
    val aWithoutSum = ∖[A, AnoSum](a, {
      def sum(items: Seq[Int]): Int = ???
    })

    // make dummy classes for convert method
    class T
    class Ta extends T

    if ({ def convert[A, B <: A](a: A) = ??? } ⊆ [AnoSum] aWithoutSum) {
      println(aWithoutSum.convert[T, Ta](new T)) // run convert, should work
    } else {
      println(s"convert is not part of $aWithoutSum")
    }

    if ({ def sum(items: Seq[Int]) = ??? } ⊆ [AnoSum] aWithoutSum) {
      aWithoutSum.sum(Seq(1, 2, 3))
    } else {
      println(s"Sum is not part of $aWithoutSum")
    }

  }
}
