package vista.operations

import scala.meta._

object Forbid {
  def apply(defn: Defn.Val): Tree = {
    val Defn.Val(_, _, tpeopt, rhs) = defn
    tpeopt.getOrElse {
      throw new IllegalArgumentException("Must provide a type")
    }

    val q"$_[..$typargs](..$args)" = rhs
    args.last match {
      case _: Term.Block => withDefs(defn)
      case _ => ???
    }
  }

  private def withDefs(defn: Defn.Val): Tree = {
    val Defn.Val(mods, param, tpeopt, expr) = defn
    val paramname = param.head
    val q"$_[..$typargs](..$args)" = expr

    val providedType = tpeopt.get

    val subjectType = typargs.head

    val traitName = Type.Name(providedType.toString)
    val className = Type.Name(s"${providedType.toString}c")

    // probably not robust
    val forbidden = {
      val q"..$stats" = args.last

      stats.map { defnn =>
        val q"..$mods def $name[..$gparams](...$paramss): $tpeopt = $_" = defnn
        val nmods = mods :+ Mod.Override()
        q"..$nmods def $name[..$gparams](...$paramss): $tpeopt = throw new NoSuchMethodException"
      }
    }

    val constructor = Ctor.Name(subjectType.toString)

    val vrr = Pat.Var.Term(Term.Name(paramname.toString))
    q"""
      trait $traitName extends $constructor {
        ..$forbidden
      }
      class $className extends ${Ctor.Name(traitName.toString)} {}
      ..$mods val $vrr = new ${Ctor.Name(className.value)}()
    """
  }
}
