package examples

import vista.lib._

@vista.enable
object ForbidEx3 {
  class A { def x = 1 }

  def f(a: A) = a.x // no error

//  def g(a: Vista[Af]) = a.x // compiler error

  def main(args: Array[String]): Unit = {
    val a  = new A
    val af = ∖[A, Af](a, { def x = ??? })
//    af.x // compiler error
    f(af)

//    g(af)
  }
}
