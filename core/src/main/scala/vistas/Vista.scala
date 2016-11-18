package vistas

import java.lang.reflect.Method

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

trait Vista {

  private lazy val forbidden: ListBuffer[Method] = ListBuffer.empty[Method]

  def isAllowed(func: Method): Boolean = !forbidden.contains(func)

  def forbid[A : ClassTag](method: String, args: Class[_]*)(implicit tag: ClassTag[A]): Unit = {
    forbidden.append(tag.runtimeClass.getMethod(method, args:_*))
  }
}

object Vista {
//  import scala.reflect.runtime.universe._
}
