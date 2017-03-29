package examples

import vista.lib._

/**
  * @author paulius
  */
@vista.enable
object ForbidEx2 {
  class A {
    def one: Int = 1
    def two: Int = 2
  }

  def mkForbid(a: A) =
    ∖[A ~> Af](a, {
      def one: Int = ???
      def two: Int = ???
    })

  def main(args: Array[String]): Unit = {
    val a = new A
    println(a.one) // should work
    println(a.two) // should work

    val af = mkForbid(a)

    try af.one
    catch {
      case e: NoSuchMethodException => println(e.getClass)
    }

    try af.two
    catch {
      case e: NoSuchMethodException => println(e.getClass)
    }

    println(af.isInstanceOf[A])

    val aAgain = ∪[A & Af ~> AA](a, af)

    try {
      println(aAgain.one)
      println(aAgain.two)
    } catch {
      case e: NoSuchElementException => println(e.getClass)
    }
  }
}
