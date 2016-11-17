import classes.B
import vistas.Vista

/**
  * @author paulius
  */
object Test extends App {

  @interceptNew
  def func(): Unit = {
    def funcc(): (Int, Int) = (1, 2)

    val b = new B
    Macros.getTypes {
      b.sayHi()
      b.sayHi(5)
      b.sayBye[Int](5)
    }
    println(b.isInstanceOf[B with Vista])
  }


  func()



//  val b = new B with Vista

//  println(classOf[B].getMethod("multi", classOf[Int], classOf[Int]))
//  println(Vista.determineMethod(b, "say", Seq(Seq(1))))
//  b.proxyMethod {
//    b.sayHi(5)
//  }
}

