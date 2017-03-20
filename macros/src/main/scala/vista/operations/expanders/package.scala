package vista.operations

import vista.operations.parsers.{OpInput, OpOverload, OpVistas}

import scala.meta.{Defn, Term}

/**
  * @author Paulius Imbrasas
  */
package object expanders {
  sealed trait Op[A]

  trait Expander[A <: OpInput, B <: Op[_]] {
    def expand(input: A): Term.Block
  }

  object Expander {
    implicit val forbidExpander: Expander[OpOverload, ForbidOp.Forbid] =
      ForbidOp.expander
    implicit val intersectExpander: Expander[OpVistas, IntersectOp.Intersect] =
      IntersectOp.expander
    implicit val unionExpander: Expander[OpVistas, UnionOp.Union] =
      UnionOp.expander
    implicit val productExpander: Expander[OpVistas, ProductOp.Product] =
      ProductOp.expander

    def apply[A <: OpInput, B <: Op[_]](implicit expander: Expander[A, B]): Expander[A, B] =
      expander
  }

  def commonMethods(inp: OpVistas,
                    lsignatures: Set[Defn.Def],
                    rsignatures: Set[Defn.Def]): Set[Defn.Def] = {
    import meta.{XDefn, XMetaIterable}

    import scala.meta._
    import scala.meta.contrib._

    /**
      * Theoretically the user should pass in normalised sets of signatures, but this is clearly not enforced.
      * Assuming they are normalised, we say def g() === def g. Unfortunately this is only true
      * for signatures. When the methods are invoked, this is no longer true.
      * This is an issue when generating the common methods between two vistas.
      * In order to solve this, the original signatures stored in the database are restored
      * which allow us to distinguish def g() and def g
      *
      * This is probably a bad idea as in theoretically this method could be used for vista \op/ visibilities
      * as well, but this particular code prohibits this from happening, since sets of visibilities are obviously
      * not stored in the database.
      */
    val db = vista.semantics.Database
    val map =
      db.get(inp.lclass).methods.map(m => m.signature.syntax   -> m).toMap ++
        db.get(inp.rclass).methods.map(m => m.signature.syntax -> m).toMap

    lsignatures.mintersect(rsignatures).map { mn =>
      val m       = map(mn.signature.syntax)
      val tparams = m.tparams.map(typ => targ"${typ.name.asType}")
      val paramss = m.paramss.map(_.map(arg => arg"${arg.name.asTerm}"))

      // FIXME: there must be a better way...
      val body = if (tparams.nonEmpty && paramss.nonEmpty) {
        q"super[${Type.Name(inp.lclass)}].${m.name}[..$tparams](...$paramss)"
      } else if (tparams.nonEmpty) {
        q"super[${Type.Name(inp.lclass)}].${m.name}[..$tparams]"
      } else if (paramss.nonEmpty) {
        q"super[${Type.Name(inp.lclass)}].${m.name}(...$paramss)"
      } else {
        q"super[${Type.Name(inp.lclass)}].${m.name}"
      }

      m.copy(body = body, mods = m.mods :+ Mod.Override())
    }
  }
}
