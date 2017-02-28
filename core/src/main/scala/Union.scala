
import vistas.Vista._

import scala.reflect.ClassTag

@vista.tratify
class X {
  private val other = 5
  val n = other
  def one(): Int = other
  def two(): Int = 2
//  def truth(): Int = n
}

@vista.tratify
class Y {
  private val other = 3
//  val n = 5
  def three(): Int = other
  def four(): Int = 4
//  def truth(): Int = n
}

/**
  * @author paulius
  */
@vista.enable
object Union {

  def acceptsXX(x: X): Unit = {
    println(x.one())
    println(x.n)
  }
//
  def acceptsYY(y: Y): Unit = {
    println(y.three())
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
    val union: XY = ∪[X, Y](x, y)

    println(union.one())
    println(union.two())
    println(union.three())
    println(union.four())

    println(acceptsX(union))
    println(acceptsY(union))

    println(acceptsXX(union))
    println(acceptsYY(union))

    val verbotten = ∖[XY](union, {
      def one(): Int = ???
    })

    println(verbotten.one())



//    trait A {
//      def f: Double = 1.0
//    }
//    trait B {
//      def f: Int = 2
//    }
//
//    val ab = new vistas.Union with A with B
//
//
//    println(ab.f)


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
