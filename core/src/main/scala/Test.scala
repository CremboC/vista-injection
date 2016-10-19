
object Test extends App {
  Macros.hello {
    class A {
      def doA(list: Seq[Int]): Boolean = {
        list.contains(1)
      }
    }
    def test(lst: Seq[Int]): Int = {
      val a = lst.map(_ + 5).sum
      a + 5
    }
    val s = Seq(1, 2, 3)
    val filtered = s.filter(_ == 1)
    val f2 = s.map(_ + 1)
    val f3 = f2 map { _ + 2 }
    val nonEmpty = filtered.nonEmpty
    val a: A = new A()
    a.doA(f3)
    test(f3)

  }
}