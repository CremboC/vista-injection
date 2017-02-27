package vista

import scala.annotation.StaticAnnotation
import scala.meta._

/**
  * @author paulius
  */
class inspect extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" = defn

    val transformed = expr.transform {
      case e@q"..$mods val $paramname: $tpeopt = $expr" =>
        println(e)
        expr match {
          case q"new { ..$stat } with ..$ctorcalls { $param => ..$stats }" =>
            val newctorCalls = ctorcalls :+ ctor"vistas.Vista"
            val rhs = q"new { ..$stat } with ..$newctorCalls { $param => ..$stats }"

            val vrr = Pat.Var.Term(Term.Name(paramname.toString))

            //            paramname.name
            println(q"..$mods val $vrr: $tpeopt = $rhs")
            q"..$mods val $vrr: $tpeopt = $rhs"
          case _ => expr
        }
      case stat => stat
    }


    val body = transformed.syntax.parse[Term].get
    val ret = q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $body"

    //    println(ret)

    ret
  }
}
