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

    println(xy.invoke[XY](f, g)(Seq(5), Seq("hi")))

    val nm = ⨯[N, M, NM](n, m)

    println(nm.invoke[NM](a, b)(Seq(1.0), Seq(0.1)))

    // endless loop
    val nmxy = ⨯[NM, XY, NMXY](nm, xy)

    println(nmxy.invoke[NMXY](a, b, f, g)(Seq(1.0), Seq(0.1), Seq(5), Seq("hi")))
  }
}
