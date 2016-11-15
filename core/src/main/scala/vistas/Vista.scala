package vistas

import java.lang.reflect.Method
import scala.reflect.runtime.{universe => ru}

trait Vista {

  private lazy val allowed: Seq[Method] = {
    // find the sayHi which takes an Int
//    val head = ru.typeOf[]
//      .members
//      .filter(_.isInstanceOf[ru.MethodSymbol])
//      .filter(_.asTerm.fullName.contains("sayHi"))
//      .filter {
//        _.typeSignature match {
//          case p: ru.MethodType =>
//            p.params.exists(_.typeSignature.resultType =:= ru.typeOf[Int])
//          case _ => false
//        }
//      }
//      .head.asInstanceOf[ru.MethodSymbol]


    Seq(classOf[classes.B].getMethod("sayHi", classOf[Int]))
  }

  def isAllowed(func: Method): Boolean = allowed.contains(func)
}

object Vista {
  import scala.reflect.runtime.{universe => ru}
  def isAllowed[T : ru.TypeTag](instance: T with Vista, funcName: String, params: List[(ru.Type, Any)]): Boolean = {
//    findMethod[T](funcName, params) match {
//      case Some(m) => instance.isAllowed[T](m)
//      case None => false
//    }

    false
  }

  private def getType[T: ru.TypeTag](obj: T) = ru.typeOf[T]

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
