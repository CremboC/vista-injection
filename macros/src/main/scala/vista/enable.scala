package vista

import scala.annotation.StaticAnnotation
import scala.meta.Term.Block
import scala.meta._

import vista.helpers.VistaHelpers._
import vista.modifiers._
import vista.operations._


class enable extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    val q"..$mods object $name extends $template" = defn
    val template"{ ..$earlyStats } with ..$ctorcalls { $param => ..$stats }" = template

    // build up SemDB
    implicit val db = semantics.Database
    defn.collect {
      case c: Defn.Class => c
    }.foreach(db.addClass)

    val q"{ .. $nstats }" = Block(stats).transform {
      case classdefn: Defn.Class => Tratify(classdefn)
    }.transform {
      // convert into vista class
      case q"..$mods val $paramname: $tpeopt = new { ..$stats } with ..$ctorCalls { ..$stats2 }" =>
        val nCtors = ctorCalls :+ ctor"${Ctor.Name("vistas.AnyV")}"
        val vrr = Pat.Var.Term(Term.Name(paramname.toString))
        q"..$mods val $vrr: $tpeopt = new { ..$stats } with ..$nCtors { ..$stats2 }"

      case valdefn@Defn.Val(_, _, _, expr) if isUnion(expr) => Unionize(valdefn)

//      case valdefn@Defn.Val(_, _, _, expr) if isForbid(expr) => Forbid(valdefn)
      case q"..$mods val $paramname: $tpeopt = $expr" if isForbid(expr) =>
        val vrr = Pat.Var.Term(Term.Name(paramname.toString))
        q"@vista.forbid ..$mods val $vrr : $tpeopt = $expr"
    }


    //    println(res)

    //    val q"{ ..$nstats }" = Block(stats).transform {
    // convert into vista class
    //      case q"..$mods val $paramname: $tpeopt = new { ..$stats } with ..$ctorCalls { ..$stats2 }" =>
    //        val nCtors = ctorCalls :+ ctor"${Ctor.Name("vistas.Vista")}"
    //        val vrr = Pat.Var.Term(Term.Name(paramname.toString))
    //        q"..$mods val $vrr: $tpeopt = new { ..$stats } with ..$nCtors { ..$stats2 }"
    //
    //      // unionise
    //      case q"..$mods val $paramname: $tpeopt = $expr" if isUnion(expr) =>
    //        val vrr = Pat.Var.Term(Term.Name(paramname.toString))
    //        q"@vista.union ..$mods val $vrr : $tpeopt = $expr"
    //
    //      // forbid
    //      case q"..$mods val $paramname: $tpeopt = $expr" if isForbid(expr) =>
    //        val vrr = Pat.Var.Term(Term.Name(paramname.toString))
    //        q"@vista.forbid ..$mods val $vrr : $tpeopt = $expr"
    //    }

    //    println(nstats)

    val ntemplate = template"{ ..$earlyStats } with ..$ctorcalls { $param => ..$nstats }"

    println(ntemplate)
    q"..$mods object $name extends $ntemplate"
  }
}

