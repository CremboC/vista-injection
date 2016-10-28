import classes.B
import vistas.Vista

/**
  * @author paulius
  */
object Test extends App {

//  class A {
//    def doA(list: Seq[Int]): Boolean = {
//      list.contains(1)
//    }
//  }
//
//  println(a)



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


  class AA {
    def m(): Unit = {

    }
  }

  class BB extends AA {
    override def m(): Unit = super.m()
  }

  class C extends BB {
    override def m(): Unit = super.m()
  }

  val c = new C
  c.m() // check .members







  import scala.reflect.runtime.{universe => ru}



//  def checker[A : ru.TypeTag, classes.B, FuncType : Function[_, _]](instance: A, function: FuncType, args: AnyRef*): Unit = {
//
//    println(ru.typeOf[A])
//  }

//  Macros.getTypes {

//  Macros.getTypes {
//    val b = new B
//    b.sayHi(5)
//  }


  @interceptNew
  def func(): Unit = {
    val b = new B
    b.sayHi()
  }

//  val ret = Macros.interceptNew {
//
//  }

//  println(ret)

//  import scala.reflect.runtime.{universe => ru}
//  def getType[T: ru.TypeTag](obj: T) = ru.typeOf[T]

//  val b = new classes.B with Vista
//  if (Vista.isAllowed[classes.B](b, "sayHi", List((ru.typeOf[Int], 5)))) {
//    b.sayHi(5)
//  }

//  val hi = Vista.check(b, b.sayHi(_: Int))
//  val bye = Vista.check(b, b.sayBye(_: Int))








//    if (b.isAllowed[classes.B](""))
// else {
//      b.sayHi()
//    }



//    b.isAllowed[classes.B]("sayHi")




//  }

}

