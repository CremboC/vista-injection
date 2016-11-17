import classes.B
import vistas.Vista

/**
  * @author paulius
  */
object Test extends App {

  @interceptNew
  def func(): Unit = {
    def funcc(): (Int, Int) = (1, 2)

//    val b = new B
    val b = new B
//    Macros.getTypes {

      b.sayHi()
      b.sayHi(5)
      b.sayBye[Int](5)


      val (x, y) = funcc()
//    }
    println(b.isInstanceOf[B with Vista])
  }


  func()



//  val b = new B
//  Vista.determineMethod(b, "sayHi", 3)
}

