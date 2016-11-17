import classes.B
import vistas.Vista

/**
  * @author paulius
  */
object Test extends App {

  class A {
    def doA(list: Seq[Int]): Boolean = {
      list.contains(1)
    }
  }

  @interceptNew
  def testt(): Unit = {



      def test(lst: Seq[Int]): Int = {
        val a = lst.map(_ + 5).sum
        a + 5
      }
      Macros.getTypes {
        val s = Seq(1, 2, 3)
        val filtered = s.filter(_ == 1)
        val f2 = s.map(_ + 1)
        val f3 = f2 map { _ + 2 }
        val nonEmpty = filtered.nonEmpty

        val a = new A

        a.doA(f3)

        thing(a)
      }

      //    test(f3)

      def thing(a: A): Unit = {
      }




      println(1)

  }



  @interceptNew
  def func(): Unit = {
    val b = new B
    Macros.getTypes {
      b.sayHi()
      b.sayHi(5)
      b.sayBye[Int](5)
    }
    println(b.isInstanceOf[B with Vista])
  }

  func()
}

