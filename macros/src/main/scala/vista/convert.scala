package vista

import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq
import scala.meta._

class convert extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val q"..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) extends $template" = defn

    val traitName = Type.Name(s"t${tname.value}")
    val classTrait = q"..$mods trait $traitName[..$tparams] extends $template"

    val quasi = ctor"${Ctor.Name(traitName.value)}"
    val template"{ ..$stats1 } with ..$ctorcalls { $param => ..$stats }" = template
    val tmpl = template"{ ..${Seq.empty} } with ..${ctorcalls :+ quasi}"
    val newClass = q"..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) extends $tmpl"

    q"""
       $classTrait
       $newClass
     """
  }
}
