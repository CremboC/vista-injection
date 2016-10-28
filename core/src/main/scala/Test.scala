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

//  val a = Macros.interceptNew {
//    val a = new A
//    a.doA(Seq(1, 2, 3))
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



  class B {
    def sayHi(a: Int): Unit = {
      println("Hi with param!")
    }

    def sayHi(): Unit = {
      println("Hi without param!")
    }
  }


  trait Vista {
    import scala.reflect.runtime.{universe => ru}

    private lazy val allowed: Seq[ru.MethodSymbol] = {
      // find the sayHi which takes an Int
      val head = ru.typeOf[B]
        .members
        .filter(_.isInstanceOf[ru.MethodSymbol])
        .filter(_.asTerm.fullName.contains("sayHi"))
        .filter {
          _.typeSignature match {
            case p: ru.MethodType =>
              p.params.exists(_.typeSignature.resultType =:= ru.typeOf[Int])
            case _ => false
          }
        }
        .head.asInstanceOf[ru.MethodSymbol]

      Seq(head)
    }

    private def getType[T: ru.TypeTag](obj: T) = ru.typeOf[T]

    def isAllowed[A : ru.TypeTag](func: ru.MethodSymbol): Boolean = allowed.contains(func)
  }

  object Vista {
    import scala.reflect.runtime.{universe => ru}
    def isAllowed[T : ru.TypeTag](instance: T with Vista, funcName: String, params: List[(ru.Type, Any)]): Boolean = {
      findMethod[T](funcName, params) match {
        case Some(m) => instance.isAllowed[A](m)
        case None => false
      }
    }

    private def findMethod[T : ru.TypeTag](funcName: String, params: List[(ru.Type, Any)]): Option[ru.MethodSymbol] = {
      val ret = ru.typeOf[T]
        .members
        .filter(_.isInstanceOf[ru.MethodSymbol])
        .map(_.asInstanceOf[ru.MethodSymbol])
        .filter(_.name == ru.TermName(funcName))
        .filter { m =>
          // get the signature of the method
          // get its parameters types, since overloads mustn't match in their parameters
          // check if the method we're looking for is the same as this one
          val methodType = m.typeSignature.asInstanceOf[ru.MethodType]
          val paramsTypes = methodType.params.map(_.typeSignature)
          paramsTypes == params.map(_._1)
        }

      ret.headOption
    }

  }

  import scala.reflect.runtime.{universe => ru}



//  def checker[A : ru.TypeTag, B, FuncType : Function[_, _]](instance: A, function: FuncType, args: AnyRef*): Unit = {
//
//    println(ru.typeOf[A])
//  }

//  Macros.getTypes {
    val b = new B with Vista
//    if (b.isAllowed[B](""))
    if (Vista.isAllowed[B](b, "sayHi", List((ru.typeOf[Int], 5)))) {
      b.sayHi(5)
    } else {
      b.sayHi()
    }



//    b.isAllowed[B]("sayHi")




//  }

}
