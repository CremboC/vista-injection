
import vistas.Vista

/**
  * @author paulius
  */
@vista.enable
object Test extends App {

  class B {
    def sayHi(a: Int): Unit = {
      println("Hi with param!")
    }

    def say(word: String): Unit = {
      println(word)
    }

    def multi(a: Int)(b: Int): Int = a + b

    def sayBye[A](a: Int): Unit = {
      println("Bye!")
    }

    def sayHi(): Unit = {
      println("Hi without param!")
    }
  }

  def func(): Unit = {
    val b = new B

//    b.forbid[B]("sayHi", classOf[Int])
//    b.forbid[B]("sayHi")

    b.sayHi()
    b.sayHi(5)
    b.sayBye[Int](5)

    println(b.isInstanceOf[B with Vista])
  }

  func()
}

