package vista

import scala.annotation.StaticAnnotation
import scala.meta._

/**
  * @author paulius
  */
class tratify extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val q"..$mods class $name extends $template" = defn
    q"..$mods trait $name extends $template"
  }
}
