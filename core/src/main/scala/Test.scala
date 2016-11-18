import classes.B
import vista.{inspect, vistacise}
import vistas.Vista

/**
  * @author paulius
  */
@vista.vistacise
object Test extends App {

  def func(): Unit = {
    val b = new B
          b.sayHi()
          b.sayHi(5)
          b.sayBye[Int](5)
    println(b.isInstanceOf[B with Vista])
  }

  func()



//  @interceptNew
//  def func(): Unit = {
//    Macros.getTypes {
//      val b = new B
//      b.sayHi()
//      b.sayHi(5)
//      b.sayBye[Int](5)
//
//      val a = 5
//      val c = a + 5
//
//      println(b.isInstanceOf[B with Vista])
//    }
//  }
//
//  def func2(): Unit = {
//    val b = new B with Vista
//    if (b.isAllowed(classOf[B].getMethod("sayHi"))) {
//      b.sayHi()
//    } else {
//      println("Not allowed")
//    }
//  }
//
//
//
//
//
//  func()
}

