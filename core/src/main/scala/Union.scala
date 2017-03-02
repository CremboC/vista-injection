
import vistas.Vista._

@vista.tratify
class X {
  private val other = 5
  val n: Int = other
  def one(): Int = other
  def two(): Int = 2
}

@vista.tratify
class Y {
  private val other = 3
  def three(): Int = other
  def four(): Int = 4
}

@vista.tratify
class F {
  def hello(): String = "Hello from F!"
}

@vista.tratify
class S {
  def a(): String = "a"
  def b(): String = "b"
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

//  def acceptsSb(sb: Sb): Boolean = sb.isInstanceOf[Sb]

  def safe[A](f: => A): Option[A] = try Option(f) catch {
    case _: Throwable => None
  }

  def sameSource(): Unit = {
    val s1 = new S
    val s2 = new S

    val s1diff: Sb = ∖[S](s1, {
      def a(): String = ???
    })

    val s2diff: Sa = ∖[S](s2, {
      def b(): String = ???
    })

    println(safe(s1diff.a()))
    println(safe(s1diff.b()))
    println(safe(s2diff.a()))
    println(safe(s2diff.b()))

    val sunion: Sab = ∪[Sa, Sb](s1diff, s2diff)
    println(safe(sunion.a()))
    println(safe(sunion.b()))
  }

  def main(args: Array[String]): Unit = {
    sameSource()
//    val x = new X
//    val y = new Y
//
//
////    println(union.one())
//    val union: XY = ∪[X, Y](x, y)
//
//    println(union.one())
//    println(union.two())
//    println(union.three())
//    println(union.four())
////
////    println(acceptsX(union))
////    println(acceptsY(union))
////
////    println(acceptsXX(union))
////    println(acceptsYY(union))
//
//    val verbotten: XYf = ∖[XY](union, {
//      def one(): Int = ???
//    })
//
////    println(acceptsXX(verbotten))
//
//    val verbotten2: XYff = ∖[XYf](union, {
//      def two(): Int = ???
//    })
//
//    try {
//      println(verbotten2.two())
//    } catch {
//      case e: Throwable => println(e.getClass)
//    }
//
//    try {
//      println(verbotten2.one())
//    } catch {
//      case e: Throwable => println(e.getClass)
//    }
//
//    val f = new F
//    val unionf: FXY = ∪[XY, F](union, f)
//
//    println(unionf.hello())
//
//    val verbottenf: FXYf = ∖[FXY](unionf, {
//      def hello(): String = ???
//    })
//
//    try {
//      println(verbottenf.hello())
//    } catch {
//      case e: Throwable => println(e.getClass)
//    }

//    println(verbotten.one())







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
