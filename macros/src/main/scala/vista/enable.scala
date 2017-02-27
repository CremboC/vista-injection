package vista

import scala.annotation.StaticAnnotation
import scala.meta._

class enable extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    val q"..$mods object $name extends $template" = defn

    def isUnion(expr: Term): Boolean = expr.tokens.syntax.contains("∪")
    def isForbid(expr: Term): Boolean = expr.tokens.syntax.contains("∖")



    val transformed = template.transform {
//      case q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" =>
//        q"@vista.inspect ..$mods def $name[..$tparams](...$paramss): $tpeopt = Macros.getTypes { $expr }"
      case q"..$mods val $paramname: $tpeopt = new { ..$stats } with ..$ctorCalls { ..$stats2 }" =>
        val nCtors = ctorCalls :+ ctor"${Ctor.Name("vistas.Vista")}"
        val vrr = Pat.Var.Term(Term.Name(paramname.toString))
        q"..$mods val $vrr: $tpeopt = new { ..$stats } with ..$nCtors { ..$stats2 }"
      case q"..$mods val $paramname: $tpeopt = $expr" if isUnion(expr) =>
        val vrr = Pat.Var.Term(Term.Name(paramname.toString))
        q"@vista.union ..$mods val $vrr : $tpeopt = $expr"
      case q"..$mods val $paramname: $tpeopt = $expr" if isForbid(expr) =>
        val vrr = Pat.Var.Term(Term.Name(paramname.toString))
        q"@vista.forbid ..$mods val $vrr : $tpeopt = $expr"
//      case s@q"$a.$b(..$argss)" =>
//        q"VistaMacros.getTypes($a.$b(..$argss))"
    }

    val ntemplate = transformed.syntax.parse[Template].get

    q"..$mods object $name extends $ntemplate"
  }
}

