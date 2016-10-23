object Test extends App {

  Macros.getTypes {
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

    val a = new A()

    a.doA(f3)
    test(f3)

    def thing(a: A): Unit = {

    }

    trait Vista {

    }

    val t = new A with Vista

    thing(t)

  }



//  @noop
//  case class Foo(a: String, b: Int)

}