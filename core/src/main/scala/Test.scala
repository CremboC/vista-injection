import classes.B
import vistas.Vista

/**
  * @author paulius
  */
object Test extends App {

  @interceptNew
  def func(): Unit = {
    def swap(a: Int, b: Int): (Int, Int) = b -> a

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

