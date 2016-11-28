package vista

import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq
import scala.meta._

class vistacise extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    // using scala.meta because it is more statically typed than scala.reflect
    val q"..$mods object $name extends $template" = defn

    val transformed = template.transform {
      case q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" =>
        q"@vista.inspect ..$mods def $name[..$tparams](...$paramss): $tpeopt = Macros.getTypes { $expr }"
    }

    val ntemplate = transformed.syntax.parse[Template].get

    q"..$mods object $name extends $ntemplate"
  }
}

