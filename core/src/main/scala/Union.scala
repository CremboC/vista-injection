
import classes.{A, B}
import vistas.Vista

/**
  * @author paulius
  */
object Union {
  class X {
    def one() = 1
    def two() = 2
    def truth() = "x"
  }

  class Y {
    def three() = 3
    def four() = 4
    def truth() = "y"
  }


  def acceptsXX(x: X): Unit = {
    println(x.truth())
  }

  def acceptsYY(y: Y): Unit = {
    println(y.truth())
  }

  def acceptsX(x: X): Boolean = x.isInstanceOf[X]
  def acceptsY(y: Y): Boolean = y.isInstanceOf[Y]

  class ∀[A, B](left: A, right: B) {
    def ∪[A, B]() = throw new RuntimeException("Should be compiled out")
  }

  object ∀ {
    def apply[A, B](left: A, right: B): ∀[A, B] = new ∀[A, B](left, right)
  }


  def ∪[A, B](left: A, right: B): Any = throw new RuntimeException("Should be compiled out")

  def main(args: Array[String]): Unit = {

    val x = new X
    val y = new Y
//    @vista.union val union = ∪(x[X], y[Y])
//    @vista.union val union = ∀(x, y).∪[X, Y]
    @vista.union val union = ∪[X, Y](x, y)
//      x[X] ∪ y[Y]

    println(union.getClass)
    println(union.isInstanceOf[Vista])
    println(acceptsX(union))
    println(acceptsY(union))
    acceptsXX(union)
    acceptsYY(union)


    println((union : X).truth())
    println(union.truth())



//    class XY
//    val union = Macros.union[X, Y](x, y)
//
//    println(union.getClass)
//
//    println(acceptsX(union))


//    val xy = {
//      object XY {
//        implicit def toX(xy: XY): X = xy.x
//        implicit def toY(xy: XY): Y = xy.y
//      }
//      class XY(val x: X, val y: Y)
//      import XY._
//      new XY(x, y)
//    }


//    val xy =

//    println(acceptsX(xy))
//    println(acceptsY(xy))



  }
}
