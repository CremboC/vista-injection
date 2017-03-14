
import vistas.Vista._

/**
  * @author paulius
  */
@vista.enable
object Test extends App {

  trait F {
    def a(): Int = 5
  }

  class G {
    def b(): Int = 3
  }

  class A {
    def nonBDef(): Unit = {
      println("Hello form A")
    }

    def common(): Int = 5
  }

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

    def common(): Int = 5
  }

  def acceptsB(b: B): Boolean = b.isInstanceOf[B]

  def func(): Unit = {
    val b = new B

    val b1: Bf = ∖[B](b, {
      def multi(a: Int)(b: Int): Int = ???
    })

    println(b1.isInstanceOf[B])
    println(acceptsB(b1))

    val a = new A
    a.common()
    val ab: AB = ∪[A, B](a, b)
    ab.nonBDef()
    ab.say("Hello from B")

    val inter: AxB = ∩[A, B](a, b)

    println(inter.common)

    val comb: FG = ⨯[F, G](f, g)
    println(comb.ab()())
  }

  func()

}

