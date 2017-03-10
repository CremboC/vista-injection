
import vistas.Vista
import vistas.Vista._

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

  def acceptsB(b: B): Boolean = b.isInstanceOf[B]

  def func(): Unit = {
    val b = new B


    val b1: Bf = ∖[B](b, {
      def multi(a: Int)(b: Int): Int = ???
    })

    println(b1.isInstanceOf[B])
    println(acceptsB(b1))
  }

  func()
}

