package vista.operations

import scala.meta._
import vista.semantics

import scala.collection.immutable


/**
  * Public interface for Forbid method
  */
object Forbid {
  import ForbidImpl._

  def apply(defn: Defn.Val)(implicit db: semantics.Database.type): Tree = parseDefn(defn) match {
    case None => q"throw new IllegalStateException" // FIXME: something more reasonable..
    case Some(de) => ForbidImpl(de)
  }

  def apply(defn: Defn.Def)(implicit db: semantics.Database.type): Tree = parseDefn(defn) match {
    case None => q"throw new IllegalStateException" // FIXME: need something more reasonable
    case Some(t) =>
      val q"{ ..$stats }" = ForbidImpl(t)
      Defn.Def(defn.mods, defn.name, defn.tparams, defn.paramss, defn.decltpe, body = Term.Block(stats))
  }
}

private[this] case class ForbidInput(
                        nclass: String,
                        oclass: String,
                        methods: Seq[Defn.Def],
                        varname: Option[String] = None,
                        lsource: Option[String] = None,
                        rsource: Option[String] = None
                      )

/**
  * Internal API of Forbid
  */
private[this] object ForbidImpl {
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

  def apply(inp: ForbidInput)(implicit db: semantics.Database.type): Tree = {
    val forbidden = inp.methods.map {
      case Defn.Def(mods, name, gparams, paramss, tpeopt, _) =>
        val nmods = mods :+ Mod.Override()
        q"..$nmods def $name[..$gparams](...$paramss): ${tpeopt.getOrElse(Type.Name("None"))} = throw new NoSuchMethodException"
    }.to[immutable.Seq]

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