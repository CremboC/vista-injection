package vista.operations.internal

import vista.operations.ForbidInput
import vista.semantics

import scala.meta._

/**
  * Internal API of Forbid
  */
private[operations] object ForbidImpl {
  def parseDefn(defn: Defn.Def): Option[ForbidInput] = defn.decltpe match {
    case None => None
    case Some(typ) =>
      val q"$_[..$typargs](..$args)" = defn.body
      val subjectType = typargs.head

      val methods = {
        val q"..$stats" = args.last
        stats.collect {
          case d: Defn.Def => d
        }
      }

      Some(ForbidInput(typ.syntax, subjectType.syntax, methods))
  }

  def parseDefn(defn: Defn.Val): Option[ForbidInput] = defn.decltpe match {
    case None => None
    case Some(typ) =>
      val q"$_[..$typargs](..$args)" = defn.rhs
      val subjectType = typargs.head

      val methods = {
        val q"..$stats" = args.last
        stats.collect {
          case d: Defn.Def => d
        }
      }

      val Defn.Val(_, param, _, _) = defn
      val paramname = param.head

      Some(ForbidInput(typ.syntax, subjectType.syntax, methods, Some(paramname.syntax)))
  }

  def apply(inp: ForbidInput)(implicit db: semantics.Database.type): Term.Block = {
    val forbidden = inp.methods.map {
      case Defn.Def(mods, name, gparams, paramss, tpeopt, _) =>
        val nmods = mods :+ Mod.Override()
        q"..$nmods def $name[..$gparams](...$paramss): ${tpeopt.getOrElse(Type.Name("None"))} = throw new NoSuchMethodException"
    }

    val constructor = Ctor.Name(inp.oclass)
    val traitq =
      q"""
         trait ${Type.Name(inp.nclass)} extends $constructor {
           ..$forbidden
         }
      """

    inp.varname match {
      case None =>
        q"""
           $traitq
           new ${Ctor.Name(inp.nclass)} {}
        """
      case Some(vr) =>
        val vrr = Pat.Var.Term(Term.Name(vr))
        q"""
          trait ${Type.Name(inp.nclass)} extends $constructor {
            ..$forbidden
          }
          val $vrr = new ${Ctor.Name(inp.nclass)} {}
        """
    }
  }
}
