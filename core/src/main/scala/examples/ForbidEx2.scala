package examples

import vista.lib._

@vista.enable
object ForbidEx2 {
  class A {
    def one: Int = 1
    def two: Int = 2
  }

  def main(args: Array[String]): Unit = {
    val a = new A

    val aone = ∖[A, Aone](a, {
      def one: Int = ???
    })

    val atwo = ∖[A, Atwo](a, {
      def two: Int = ???
    })

//    println(aone.one)
//    println(atwo.two)
//    af.one // compiler error
//    af.two // compiler error
//
//    val partial = aone.one _ // compiler error
//
//    println(af.isInstanceOf[A])

    val aAgain = ∪[Aone, Atwo, AA](aone, atwo)
    println(aAgain.one)
    println(aAgain.two)

//    try {
//      println(aAgain.one)
//      println(aAgain.two)
//    } catch {
//      case e: NoSuchElementException => println(e.getClass)
//    }
  }
}
