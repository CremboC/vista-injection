package examples

import vista.lib._

@vista.enable
object ProductEx {
  class X {
    def f[A <: Int](i: A): Int = i * 2
  }

  class Y {
    def g[A <: String](s: A): Char = s.head
  }

  class N {
    def a(l: Double): Double = l
  }

  class M {
    def b(l: Double): Double = l
  }

  class A {
    def m: Int = 1
  }

  def main(args: Array[String]): Unit = {
    val x = new X
    val y = new Y
    val m = new M
    val n = new N

    val xy = ⨯[X, Y, XY](x, y)

    println(xy.f(5).g("hi"))

    val nm = ⨯[N, M, NM](n, m)

    println(nm.a(1.0).b(0.1))

    val nmxy = ⨯[NM, XY, NMXY](nm, xy)

    val g: ((Double, Double), (Int, Char)) = nmxy.a(1.0).b(0.1).f(5).g("hi")
    println(g)

    val a     = new A
    val anmxy = ∪[A, NMXY, AuNMXY](a, nmxy)

  }
}
