package examples

import vista.lib._

import scala.util.Random

@vista.enable
object StateInconsistency {
  class A(val f: Int) {
    private val xf = f + Random.nextInt(10)
    def x: Int     = 1 - xf
  }
  class B(val g: Int) { def y: Int = 2 }

  def main(args: Array[String]): Unit = {
    val a = new A(1)
    val b = new B(2)

    val ab = ∪[A, B, AB](a, b)
    require(ab.x == a.x, () => "ab.x is not a.x")
    require(ab.f == a.f, () => "ab.f is not a.f")
  }
}
