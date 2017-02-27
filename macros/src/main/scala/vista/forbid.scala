package vista

import scala.annotation.StaticAnnotation
import scala.meta._

/**
  * Assume
  * {{{
  * class X {
  *   def a
  *   def b
  * }
  *
  *
  * val x = new X
  * }}}
  *
  * Usage is
  * {{{
  * val partial = ∖[X](x, {
  *   def one(): Int = ???
  * })
  * }}}
  *
  * Generated code is
  * {{{
  * class Xp extends X {
  *   override def one(): Int = throw new RuntimeException
  * }
  * val partial = new Xp
  * }}}
  */
class forbid extends StaticAnnotation {
  // issues
  // 1. assumes purity of functions.
  // 2. ignores members

  inline def apply(defn: Any): Any = meta {
    val q"..$mods val $paramname: $tpeopt = $expr" = defn
    val q"$forbidOp[..$typargs](..$args)" = expr

    val subjectType = typargs.head
    val subjectVar = args.head

    println(subjectType)

    val forbidName = Term.fresh("forbid")
    val forbidType = Type.Name(forbidName.value)

    val forbidden = {
      val q"..$stats" = args.last

      stats.init.map { defnn =>
        val q"..$mods def $name[..$gparams](...$paramss): $tpeopt = $_" = defnn
        val nmods = mods :+ Mod.Override()
        q"..$nmods def $name[..$gparams](...$paramss): $tpeopt = throw new NoSuchMethodException"
      }
    }

    val constructor = {
      val name = Ctor.Name(subjectType.toString)
//      ctor"$name($subjectVar)"
      ctor"$name"
    }

    val vrr = Pat.Var.Term(Term.Name(paramname.toString))
    q"""
      if (${Term.Name(subjectType.toString)}.isInstanceOf[vistas.Union]) {

      }
      class $forbidType extends $constructor {
        ..$forbidden
      }
      val $vrr = new ${Ctor.Name(forbidName.value)}()
    """
  }
}

