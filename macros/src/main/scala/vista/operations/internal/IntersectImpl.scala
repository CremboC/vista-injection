package vista.operations.internal

import vista.operations.parsers.IntersectInput
import vista.semantics

import scala.meta._
import scala.meta.contrib._
import scala.collection.immutable.Seq

/**
  * @author Paulius Imbrasas
  */
private[operations] object IntersectImpl {
  import meta.XMetaIterable

  def apply(inp: IntersectInput)(implicit db: semantics.Database.type): Term.Block = {
    val traitName = Type.Name(inp.newtype)

    val leftTypeCtor = Ctor.Name(inp.lclass)
    val rightTypeCtor = Ctor.Name(inp.rclass)

    val lclazz = db.get(inp.lclass)
    val rclazz = db.get(inp.rclass)

    // we're overriding only the methods that no longer allow
    // mintersect will return a list of methods which are allow, but
    // we can only disallow methods, hence that is used here
    val forbidden = lclazz.methods.mdiff(rclazz.methods).map { m =>
      m.copy(body = q"throw new NoSuchMethodException", mods = m.mods :+ Mod.Override())
    }.toSeq

    inp.newvar match {
      case None => ???
      case Some(nvar) =>
        q"""
           trait $traitName extends $leftTypeCtor with $rightTypeCtor {
             ..${forbidden.asInstanceOf[Seq[Stat]]}
           }
           val ${Term.Name(nvar).asPat} = new ${traitName.asCtorRef} {}
        """
    }
  }
}
