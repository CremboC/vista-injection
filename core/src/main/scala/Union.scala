
import vistas.Vista._

/**
  * @author paulius
  */
@vista.vistacise
object Union {

  class X { _ : vistas.Vista =>
    val n = 3
    def one() = 1
    def two() = 2
    def truth(): Int = n
  }

  class Y { _ : vistas.Vista =>
    val n = 5
    def three() = 3
    def four() = 4
    def truth(): Int = n
  }

  def acceptsXX(x: X): Unit = {
    println(x.one())
    println(x.truth())
  }

  def acceptsYY(y: Y): Unit = {
    println(y.truth())
  }

  def acceptsX(x: X): Boolean = x.isInstanceOf[X]
  def acceptsY(y: Y): Boolean = y.isInstanceOf[Y]

  def main(args: Array[String]): Unit = {
    val x = new X
    val y = new Y

//    val union = new tX with tY
//    println(union.one())
    val union: XY = ∪[X, Y](x, y)

    union.forbid[X]("one")


    println(union.one())
    println(union.truth())


//    val partial1 = ∖[XY](union, {
//      def one(): Int = ???
//    })
//
//    println(partial1.one())
//
//    val partial = ∖[X](x, {
//      def one(): Int = ???
//    })
//
//    println(partial.one())
//    println(partial.two())
//
//    val nunion = ∖[XY](union, {
//      def one(): Int = ???
//    })

//    println(union.getClass)
//    println(union.isInstanceOf[Vista])
//    println(acceptsX(union))
//    println(acceptsY(union))
//    acceptsXX(union)
//    acceptsYY(union)
//
//
//    println((union : X).truth())
//    println(union.truth())

//    val x = new X
    // remove one from x
//    class Xp extends X {
//      override def one(): Int = throw new RuntimeException("Forbidden")
//      override def two(): Int = super.two()
//      override def truth(): String = super.truth()
//    }
//    val partial = new Xp

//    println(partial.truth())

//    println(acceptsX(partial))
//    println(acceptsY(partial))
//    println(partial.isInstanceOf[Vista])
  }
}
