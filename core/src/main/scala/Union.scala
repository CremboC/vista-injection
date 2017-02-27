
import vistas.Vista._

import scala.reflect.ClassTag

@vista.tratify
class X {
  val n = 3
  def one() = 1
  def two() = 2
  def truth(): Int = n
}

@vista.tratify
class Y {
  val n = 5
  def three() = 3
  def four() = 4
  def truth(): Int = n
}

/**
  * @author paulius
  */
@vista.enable
object Union {

  def acceptsXX(x: X): Unit = {
    println(x.one())
    println(x.truth())
  }

  def acceptsYY(y: Y): Unit = {
    println(y.truth())
  }

  def acceptsX(x: X): Boolean = x.isInstanceOf[X]
  def acceptsY(y: Y): Boolean = y.isInstanceOf[Y]

  def getClass[A](a: A)(implicit tag: ClassTag[A]): Class[_ <: A] = {
    a.getClass
  }

  def main(args: Array[String]): Unit = {
    val x = new X
    val y = new Y

//    val union = new tX with tY
//    println(union.one())
//    val union: XY = ∪[X, Y](x, y)


    trait A {
      def f = 1
    }
    trait B {
      def f = 2
    }

    val ab = new vistas.Union with A with B


    println(ab.f)


//    println(union.isInstanceOf[_ <: X])

//    x.forbid[X]("one")
//
//    println(getClass(x).getMethod("one"))


//    union.forbid[X]("one")
////    VistaMacros.getTypes()
//
//
//    VistaMacros.getTypes(union.one().+(5))
//    VistaMacros.getTypes(union.one())

//    println(union.one())
//    println(union.truth())
  }
}
