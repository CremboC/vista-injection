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

//  def union[A : ClassTag, B : Vista]()(implicit tag: ClassTag[A]): A with B with Vista = {
////    tag.runtimeClass.getConstructor().
//  }
}



object Vista {
  //  union
  def ∪[A, B](left: A, right: B): Any = throw new RuntimeException("Should be compiled out")

  // intersection
  def ∩[A, B](left: A, right: B): Any = throw new RuntimeException("Should be compiled out")

  // difference
  def ∖[A](left: A, arg: Any): Any = throw new RuntimeException("Should be compiled out")

  // product
  def ⨯[A, B](left: A, right: B): Any = throw new RuntimeException("Should be compiled out")
}
