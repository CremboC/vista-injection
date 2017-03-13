package vista.operations.expanders

import vista.operations.parsers.OpOverload
import vista.semantics

import scala.meta._
import scala.collection.immutable.Seq

/**
  * Internal API of Forbid
  */
private[operations] object ForbidOp {
  type Forbid = Op[ForbidOp.type]

  private implicit val db = semantics.Database

  val expander: Expander[OpOverload, Forbid] = (inp: OpOverload) => {
    val forbidden = inp.methods.map {
      case Defn.Def(mods, name, gparams, paramss, tpeopt, _) =>
        val nmods = mods :+ Mod.Override()
        q"..$nmods def $name[..$gparams](...$paramss): ${tpeopt.getOrElse(Type.Name("None"))} = throw new NoSuchMethodException"
    }.asInstanceOf[Seq[Stat]]

    val constructor = Ctor.Name(inp.lclass)
    val traitq =
      q"""
         trait ${Type.Name(inp.newtype)} extends $constructor {
           ..$forbidden
         }
      """

    inp.newvar match {
      case None =>
        q"""
           $traitq
           new ${Ctor.Name(inp.newtype)} {}
        """
      case Some(vr) =>
        val vrr = Pat.Var.Term(Term.Name(vr))
        q"""
          trait ${Type.Name(inp.newtype)} extends $constructor {
            ..$forbidden
          }
          val $vrr = new ${Ctor.Name(inp.newtype)} {}
        """
    }
  }
}
