package vistas

import java.lang.reflect.Method

import scala.reflect.ClassTag

trait Vista {

  private lazy val forbidden: Seq[Method] = {
    Seq(classOf[classes.B].getMethod("sayHi", classOf[Int]))
  }

  def isAllowed(func: Method): Boolean = !forbidden.contains(func)

  def proxyMethod[A](method: => A): A = {
    method
  }
}

object Vista {
  import scala.reflect.runtime.universe._

//  val m = runtimeMirror(getClass.getClassLoader)

  private def getClass[T : ClassTag](v: T)(implicit tag: ClassTag[T]): Class[_] = {
    println(tag)
    tag.runtimeClass
  }

  def determineMethod[T, R <: Any : TypeTag](instance: T, name: String, variables: R*): Method = {
//    println(typeOf[R])
//    val clazz: Class[_] = m.runtimeClass(typeOf[R].typeSymbol.asClass)
//    println(clazz)

    val types = variables.map {
      case _: Int => classOf[Int]
      case _: Double => classOf[Double]
      case _: String => classOf[String]
      case v: Any => getClass(v)
    }

    println(types)
    instance.getClass.getMethod(name, types:_*)
  }
}
