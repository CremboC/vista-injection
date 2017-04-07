package vista

import vista.internal.Enable

import scala.annotation.StaticAnnotation
import scala.meta._

class enable extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case obj: Defn.Object => Enable(obj)
      case clazz: Defn.Class => Enable(clazz)
      case _ => abort("This annotation must be placed on an object or a class")
    }
  }
}
