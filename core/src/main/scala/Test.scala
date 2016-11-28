import classes.B
import vistas.Vista

/**
  * @author paulius
  */
@vista.vistacise
object Test extends App {

  def func(): Unit = {
    val b = new B

    b.forbid[B]("sayHi", classOf[Int])
    b.forbid[B]("sayHi")

    b.sayHi()
    b.sayHi(5)
    b.sayBye[Int](5)

    println(b.isInstanceOf[B with Vista])
  }

  func()


}

