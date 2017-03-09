package vista

import vista.operations.Forbid

import scala.annotation.StaticAnnotation
import scala.meta._

class forbid extends StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
//    implicit val db = vista.semantics.Database
    val q"..$mods val $paramname: $tpeopt = $expr" = defn

//    println(defn)


//    Forbid(Defn.Val(mods, paramname, tpeopt.asInstanceOf[Some[Type]], expr))

//    val q"..$mods val $paramname: $tpeopt = $expr" = defn
    val q"$_[..$typargs](..$args)" = expr

    val providedType = tpeopt.asInstanceOf[Some[Type]].getOrElse {
      throw new IllegalArgumentException("Must provide a type")
    }

    val subjectType = typargs.head

    val traitName = Type.Name(providedType.toString)
    val className = Type.Name(s"${providedType.toString}c")

    // probably not robust
    val forbidden = {
      val q"..$stats" = args.last

      stats.init.map { defnn =>
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

