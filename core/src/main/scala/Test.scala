
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

//    val b1 = {
//      trait Bf extends B {
//        override def multi(a: Int)(b: Int): Int = throw new NoSuchElementException
//      }
//      class Bfc extends Bf
//      new Bfc()
//    }

//    b1.isInstanceOf[B]


    val b1: Bf = ∖[B](b, {
      def multi(a: Int)(b: Int): Int = ???
    })

//    b1.multi(5)(5)
    println(b1.isInstanceOf[B])

    println(acceptsB(b1))

//    val bunion: BfUB = ∪[B, Bf](b, b1)

//    println(bunion)


//    b.forbid[B]("sayHi", classOf[Int])
//    b.forbid[B]("sayHi")

    b.sayHi()
    b.sayHi(5)
    b.sayBye[Int](5)

    println(b.isInstanceOf[B with Vista])
  }

  func()
}

