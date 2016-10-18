object Test extends App {
  Macros.hello {
    def test(): Int = {
      val a = 1 + 2
      a + 5
    }
    val s = Seq(1, 2, 3)
    val filtered = s.filter(_ == 1)
    val f2 = s.map(_ + 1)
    val f3 = f2 map { _ + 2 }
    val nonEmpty = filtered.nonEmpty
    nonEmpty
  }


}