package vista.operations.expanders

import vista.operations.parsers.OpVistas
import vista.semantics

import scala.collection.immutable.Seq
import scala.meta._
import _root_.meta.XDefnIterable

import scala.meta.contrib._

/**
  * @author Paulius Imbrasas
  */
private[operations] object IntersectOp {
  type Intersect = Op[IntersectOp.type]
  import meta.XMetaIterable

  private implicit val db = semantics.Database

  val expander: Expander[OpVistas, Intersect] = (inp: OpVistas) => {
    val traitName = Type.Name(inp.newtype)

    val leftTypeCtor  = Ctor.Name(inp.lclass)
    val rightTypeCtor = Ctor.Name(inp.rclass)

    val lclazz = db(inp.lclass)
    val rclazz = db(inp.rclass)

    // we're overriding only the methods that no longer allow
    // mintersect will return a list of methods which are allow, but
    // we can only disallow methods, hence that is used here
    val methodsInBoth = lclazz.methods >+< rclazz.methods
    val forbidden = methodsInBoth.map { m =>
      m.copy(body = q"throw new NoSuchMethodException", mods = m.mods :+ Mod.Override())
    }

    // get common signatures in order to avoid "trait X inherits conflicting members"
    val lsignatures = lclazz.methods.signatures
    val rsignatures = rclazz.methods.signatures

    val common = commonMethods(inp, lsignatures, rsignatures)

    val result = {
      forbidden ++ common
    }.toSeq.sortBy(_.name.syntax).asInstanceOf[Seq[Stat]]

    inp.newvar match {
      case None => ???
      case Some(nvar) =>
        q"""
           trait $traitName extends $leftTypeCtor with $rightTypeCtor {
             ..$result
           }
           val ${Term.Name(nvar).asPat} = new ${traitName.asCtorRef} {}
        """
    }
  }
}
