
import vistas.Vista._

/**
  * @author paulius
  */
@vista.enable
object Union {

  class X {
    val n = 3
    def one() = 1
    def two() = 2
    def truth(): Int = n
  }

  class Y {
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
  }
}
