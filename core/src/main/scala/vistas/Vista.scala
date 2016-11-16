package vistas

import java.lang.reflect.Method
import scala.reflect.runtime.{universe => ru}

trait Vista {

  private lazy val forbidden: Seq[Method] = {
    Seq(classOf[classes.B].getMethod("sayHi", classOf[Int]))
  }

  def isAllowed(func: Method): Boolean = !forbidden.contains(func)
}
