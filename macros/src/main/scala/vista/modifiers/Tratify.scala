package vista.modifiers

import vista.semantics
import meta.XCtor

import scala.meta._
import scala.meta.contrib._

/**
  * @author Paulius Imbrasas
  */
object Tratify {
  private implicit val db = semantics.Database

  def apply(cls: Defn.Class): Tree = {
    val traite = Defn.Trait(cls.mods, cls.name, cls.tparams, XCtor.default, cls.templ)

    val ntrait =
      if (cls.ctor.paramss.isEmpty) traite
      else {
        val paramss = cls.ctor.paramss

        def extractDecltpe(p: Term.Param): Type.Name = p.decltpe match {
          case None => Type.Name("") // will probably fail here if ever reached.
          case Some(typ) => Type.Name(typ.syntax)
        }

        // convert params into val/var declarations
        // which will be replaced inside the body of the trait
        val decls = paramss.flatMap(_.map { p =>
          val decl = p.mods match {
            case Nil =>
              q"private val ${p.name.asPat}: ${extractDecltpe(p)}"
            case Mod.ValParam() :: Nil =>
              q"val ${p.name.asPat}: ${extractDecltpe(p)}"
            case Mod.VarParam() :: Nil =>
              q"var ${p.name.asPat}: ${extractDecltpe(p)}"
            case Mod.Private(_) :: Mod.ValParam() :: Nil =>
              q"private val ${p.name.asPat}: ${extractDecltpe(p)}"
            case Mod.Private(_) :: Mod.VarParam() :: Nil =>
              q"private var ${p.name.asPat}: ${extractDecltpe(p)}"
            case Mod.Final() :: Mod.ValParam() :: Nil =>
              q"val ${p.name.asPat}: ${extractDecltpe(p)}"
            case Mod.Final() :: Mod.VarParam() :: Nil =>
              q"var ${p.name.asPat}: ${extractDecltpe(p)}"
          }
          decl
        })

        val nstats = traite.templ.stats match {
          case None => decls
          case Some(stats) => decls ++ stats
        }

        traite.copy(templ = traite.templ.copy(stats = Option(nstats)))
      }

    ntrait
  }
}
