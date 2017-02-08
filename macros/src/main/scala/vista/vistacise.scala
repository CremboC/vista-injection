package vista

import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq
import scala.meta._

class vistacise extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    // using scala.meta because it is more statically typed than scala.reflect
    val q"..$mods object $name extends $template" = defn

    def isUnion(expr : Term): Boolean = expr.tokens.syntax.contains("∪")
    def isForbid(expr : Term): Boolean = expr.tokens.syntax.contains("∖")

    val transformed = template.transform {
//      case q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" =>
//        q"@vista.inspect ..$mods def $name[..$tparams](...$paramss): $tpeopt = Macros.getTypes { $expr }"
      case q"..$mods val $paramname: $tpeopt = $expr" if isUnion(expr) =>
        val vrr = Pat.Var.Term(Term.Name(paramname.toString))
        q"@vista.union ..$mods val $vrr : $tpeopt = $expr"
      case q"..$mods val $paramname: $tpeopt = $expr" if isForbid(expr) =>
        val vrr = Pat.Var.Term(Term.Name(paramname.toString))
        q"@vista.forbid ..$mods val $vrr : $tpeopt = $expr"
    }

    val ntemplate = transformed.syntax.parse[Template].get

    q"..$mods object $name extends $ntemplate"
  }
}

