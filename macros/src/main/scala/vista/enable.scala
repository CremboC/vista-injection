package vista

import scala.annotation.StaticAnnotation
import scala.meta.Term.Block
import scala.meta._

object EnableHelpers {
  implicit class NonRecursiveTransformer(tree: Tree) {
    def transformNR(fn: PartialFunction[Tree, Tree]): Tree = {
      object transformer extends Transformer {
        override def apply(tree: Tree): Tree = {
          if (fn.isDefinedAt(tree)) fn(tree)
          else super.apply(tree)
        }
      }
      transformer(tree)
    }
  }
}

class enable extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    import EnableHelpers.NonRecursiveTransformer
    val q"..$mods object $name extends $template" = defn
    val template"{ ..$earlyStats } with ..$ctorcalls { $param => ..$stats }" = template

    def isUnion(expr: Term): Boolean = {
      val syntax = expr.syntax
      syntax.contains("∪")
//      || syntax.contains("sum")
    }
    def isForbid(expr: Term): Boolean = {
      val syntax = expr.syntax
      syntax.contains("∖")
//      || syntax.contains("diff")
    }

    val q"{ ..$nstats }" = Block(stats).transform {
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
    } /*transformNR {
         case q"$a.$b(..$argss)" =>
           q"VistaMacros.getTypes($a.$b(..$argss))"
       }*/

    val ntemplate = template"{ ..$earlyStats } with ..$ctorcalls { $param => ..$nstats }"
    q"..$mods object $name extends $ntemplate"
  }
}

