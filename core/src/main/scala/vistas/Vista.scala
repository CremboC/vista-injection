package vistas

import java.lang.reflect.Method

import scala.reflect.ClassTag

trait Vista {

  private lazy val forbidden: Seq[Method] = {
    Seq(classOf[classes.B].getMethod("sayHi", classOf[Int]))
  }

  def isAllowed(func: Method): Boolean = !forbidden.contains(func)
}

object Vista {
  import scala.reflect.runtime.universe._

  private def getClass[T : ClassTag](v: T)(implicit tag: ClassTag[T]): Class[_] = {
    println(tag)
    tag.runtimeClass
  }

  def determineMethod[T : ClassTag](instance: T, name: String, variables: Any*)(implicit tag: ClassTag[T]): Method = {
//    println(classOf[Int])
    val types = variables.map(v => getClass(v))
    println(types)

//    println(tag)

//    println(tag.runtimeClass)
    tag.runtimeClass.getMethod(name, types:_*)
  }
}
