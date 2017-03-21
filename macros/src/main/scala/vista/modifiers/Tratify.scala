package vista.modifiers

import meta.xtensions._
import vista.semantics
import vista.semantics.Inst

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.contrib._

/**
  * @author Paulius Imbrasas
  */
object Tratify {
  private val db = semantics.Database

  // val a = new A("hi")
  //
  // ~~>
  //
  // val a = new A {
  //  override val x = "hi"
  // }
  def apply(term: Term.New): Term.New = {
    val ts = term.templ.ctorsWithArguments.filter(c => db.exists(c.fun.syntax))
    ts match {
      case None       => term
      case Some(ctor) =>
        // FIXME: in this case the first constructor must be a class->trait convertee
        val clazz = db(ctor.fun.syntax) match {
          case t: Inst.Class => t
          case _             => throw new IllegalArgumentException("Must be a class converted into a trait")
        }

        val members = clazz.ctorMembers.zip(ctor.args).map {
          case (member, arg) =>
            val value = s"${arg.syntax}".parse[Term].get
            member match {
              case m: Defn.Val => m.copy(mods = m.mods :+ Mod.Override(), rhs = value)
              case m: Defn.Var => m.copy(mods = m.mods :+ Mod.Override(), rhs = Some(value))
            }
        }

        val ctorCalls = term.templ.parents.tail

        q"""
          new ${Ctor.Name(ctor.fun.syntax)} with ..$ctorCalls {
            ..$members
          }
       """
    }
  }

  // FIXME: probably wrong
  def combineCtorMembers(cls: Inst.Class): Seq[Defn] =
    cls.ctorMembers.zip(cls.ctor.paramss.flatten).map {
      case (member, arg) =>
        val value = s"${arg.syntax}".parse[Term].get
        member match {
          case m: Defn.Val => m.copy(mods = m.mods :+ Mod.Override(), rhs = value)
          case m: Defn.Var => m.copy(mods = m.mods :+ Mod.Override(), rhs = Some(value))
        }
    }

  def ctorToDecls(cls: Defn.Class): Seq[Decl] =
    if (cls.ctor.paramss.isEmpty) Seq.empty
    else {
      val paramss = cls.ctor.paramss

      def extractDecltpe(p: Term.Param): Type.Name = p.decltpe match {
        case None =>
          Type.Name("") // will probably fail here if ever reached.
        case Some(typ) => Type.Name(typ.syntax)
      }

      // convert params into val/var declarations
      // which will be replaced inside the body of the trait
      val decls = paramss.flatMap(_.map { p =>
        val decl = p.mods match {
          case Nil =>
            q"val ${p.name.asPat}: ${extractDecltpe(p)}"
          case Mod.ValParam() :: Nil =>
            q"val ${p.name.asPat}: ${extractDecltpe(p)}"
          case Mod.VarParam() :: Nil =>
            q"var ${p.name.asPat}: ${extractDecltpe(p)}"
          case Mod.Private(_) :: Mod.ValParam() :: Nil =>
            q"val ${p.name.asPat}: ${extractDecltpe(p)}"
          case Mod.Private(_) :: Mod.VarParam() :: Nil =>
            q"var ${p.name.asPat}: ${extractDecltpe(p)}"
          case Mod.Final() :: Mod.ValParam() :: Nil =>
            q"val ${p.name.asPat}: ${extractDecltpe(p)}"
          case Mod.Final() :: Mod.VarParam() :: Nil =>
            q"var ${p.name.asPat}: ${extractDecltpe(p)}"
        }
        decl
      })

      decls
    }

  def apply(cls: Defn.Class): Defn.Trait = {
    val traite =
      Defn.Trait(cls.mods, cls.name, cls.tparams, XCtor.default, cls.templ)

    val decls = ctorToDecls(cls)
    if (decls.isEmpty) traite
    else {
      val nstats = traite.templ.stats match {
        case None        => decls
        case Some(stats) => decls ++ stats
      }

      traite.copy(templ = traite.templ.copy(stats = Option(nstats)))
    }
  }
}
