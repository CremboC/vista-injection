import vistas.Vista

/**
  * Created by Crembo on 2017-02-06.
  */
object Main {
  trait X {
    def one() = 1
    def two() = 2
  }

  trait Y {
    def three() = 3
    def four() = 4
  }

  def acceptsX(x: X): Boolean = x.isInstanceOf[X]
  def acceptsY(y: Y): Boolean = y.isInstanceOf[Y]

  def main(args: Array[String]): Unit = {
    val xy = new X with Y with Vista {
      override def one(): Int = if (isAllowed(classOf[X].getMethod("one"))) super.one() else throw new RuntimeException("No")
    }
    xy.forbid[X]("one")

    println(xy.one())
//    val xy = new Vista {
//      private val x = new X
//      private val y = new Y
//
//      def one() = x.one()
//      def two() = x.two()
//      def three() = y.three()
//      def four() = y.four()
//    }


    println(acceptsX(xy)) // should work, but doesn't
    println(acceptsY(xy)) // should work, but doesn't
  }
}
