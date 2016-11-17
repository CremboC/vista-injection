import scala.annotation.StaticAnnotation
import scala.collection.immutable._
import scala.meta._

class interceptNew extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" = defn
    val q"{ ..$stats }" = expr

//    val vistaTerms = mutable.ListBuffer[Pat.Var.Term]()

    val changeStats = stats.map {
      case stat @ q"..$mods val $paramname: $tpeopt = $expr" =>
        expr match {
          case q"new { ..$stat } with ..$ctorcalls { $param => ..$stats }" =>
            println(stat, ctorcalls, param, stats)

            val newctorCalls = ctorcalls :+ ctor"vistas.Vista"
            val rhs = q"new { ..$stat } with ..$newctorCalls { $param => ..$stats }"
            val vrr = Pat.Var.Term(Term.Name("b"))

//            vistaTerms.append(vrr)

            q"..$mods val $vrr: $tpeopt = $rhs"
          case _ => expr
        }
      case stat @ q"$expr(...$aexprssnel)" =>
        expr match {
          case q"$variable.$method[..$generics]" =>
//            println(variable, method, generics)
            //              q"$expr(...$aexprssnel)"
            stat
          case q"$variable.$method" =>
            val vrr = Pat.Var.Term(Term.fresh("vistaAllowed"))
//            val args = Term.Arg.Named(Term.Name("func"), Term.Name(method.value))

            val methodName = Lit(method.value)
//            val intType = arg"classOf[Int]"

            val params: Seq[Term.Arg] = if (aexprssnel.head.nonEmpty) {
              aexprssnel.head.map(p => p)
            } else Seq[Term.Arg]()


//            val $vrr = classOf[classes.B].getMethod($methodName, $intType)
            q"""
                val $vrr = Vista.determineMethod($variable, ${methodName}, ..$params)
                if ($variable.isAllowed(${vrr.name})) {
                  $stat
                } else {
                  println("No")
                }
             """
          case _ => stat
        }
      case stat => stat
    }

    val body = changeStats
    val ret = q"..$mods def $name[..$tparams](...$paramss): $tpeopt = { ..$body }"

    println(ret)

    ret
  }
}
