package vista.operations.internal

import vista.operations.parsers.ForbidInput
import vista.semantics

import scala.meta._

/**
  * Internal API of Forbid
  */
private[operations] object ForbidImpl {

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
