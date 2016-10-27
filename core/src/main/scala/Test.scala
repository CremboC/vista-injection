import vistas.Vista

/**
  * @author paulius
  */
object Test extends App {

  class A {
    def doA(list: Seq[Int]): Boolean = {
      list.contains(1)
    }
  }

  val a = Macros.interceptNew {
    val a = new A
    a.doA(Seq(1, 2, 3))
  }

  println(a)



//  Macros.getTypes {
//
//
//    def test(lst: Seq[Int]): Int = {
//      val a = lst.map(_ + 5).sum
//      a + 5
//    }
//    val s = Seq(1, 2, 3)
//    val filtered = s.filter(_ == 1)
//    val f2 = s.map(_ + 1)
//    val f3 = f2 map { _ + 2 }
//    val nonEmpty = filtered.nonEmpty
//
//    val a = new A
//
//    a.doA(f3)
//    test(f3)
//
//    def thing(a: A): Unit = {
//
//
//    }
//
//
//    thing(a)
//
//    println(1)
//  }



//  @noop
//  case class Foo(a: String, b: Int)

  import scala.reflect.runtime.{universe => ru}

  class B {
    def sayHi(a: Int): Unit = {
      println("Hi!")
    }
  }

  trait Vista {
    private val allowed = Seq("sayHi")
    def isAllowed[A : ru.TypeTag](func: String): Boolean = {
      val method: ru.MethodSymbol = ru.typeOf[A].member(ru.TermName(func)).asMethod
      true
    }
  }


  def checker[A : ru.TypeTag, B, FuncType : Function](instance: A, function: FuncType, args: AnyRef*): Unit = {

    println(ru.typeOf[A])
  }

  val b = new B with Vista
  checker(b, b.sayHi)
}
