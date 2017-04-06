package examples

import vista.lib._

@vista.enable
object ProductEx {
  class X {
    def f(i: Int): Int = i * 2
  }

  class Y {
    def g(s: String): Char = s.head
  }

  class N {
    def a(l: Double): Double = l
  }

  class M {
    def b(l: Double): Double = l
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

//    val xynm = ⨯[Vista[NM], Vista[XY], NMXY](nm, xy) // infinite loop?
//    println(xynm.f(5).g("hi").a(1.0).b(0.1))
  }
}
