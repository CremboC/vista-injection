package vista.operations.expanders

import vista.meta.xtensions._
import vista.operations.parsers.{OpOverload, OpVistas}
import vista.semantics
import vista.semantics.Database.ClassName

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.contrib._

/**
  * Internal API of Forbid
  */
private[operations] object ForbidOp {
  type Forbid = Op[ForbidOp.type]

  private val db = semantics.Database

  val vistasExpander: Expander[OpVistas, Forbid] = (inp: OpVistas) => {
    val lclazz = db(inp.lclass)
    val rclazz = db(inp.rclass)

    val lmethods = lclazz.visibilities
    val rmethods = rclazz.visibilities

    val allowed    = lmethods.signatures \ rmethods.signatures
    val disallowed = (lmethods ++ rmethods).signatures \ allowed

    val forbiddenDefns = {
      def visibilitiesMap(className: ClassName): Map[ClassName, Defn.Def] =
        db.get(className).visibilities.map(m => m.signature.syntax -> m).toMap

      val map = visibilitiesMap(inp.lclass) ++ visibilitiesMap(inp.rclass)

      disallowed.map { defn =>
        val defnn = map(defn.syntax)
        defnn.copy(mods = (defn.mods :+ Mod.Override()).toSet.to,
                   body = q"throw new NoSuchMethodException")
      }
    }

    val members = ctorMembersDefns(lclazz, inp.lvar)

    inp.newvar match {
      case None => ???
      case Some(nvar) =>
        q"""
          trait ${Type.Name(inp.newtype)} extends ${Ctor.Name(inp.lclass)} with ${Ctor.Name(
          inp.rclass)} {
            ..${forbiddenDefns.toSeq.asInstanceOf[Seq[Stat]]}
          }
          val ${Term.Name(nvar).asPat} = new ${Ctor.Name(inp.newtype)} {
            ..$members
          }
        """
    }
  }

  val expander: Expander[OpOverload, Forbid] = (inp: OpOverload) => {
    val forbidden = inp.methods
      .map { defn =>
        defn.copy(mods = (defn.mods :+ Mod.Override()).toSet.to,
                  body = q"throw new NoSuchMethodException")
      }
      .asInstanceOf[Seq[Stat]]

    val constructor = Ctor.Name(inp.lclass)

    val lclazz  = db(inp.lclass)
    val members = ctorMembersDefns(lclazz, inp.lvar)

    inp.newvar match {
      case None =>
        q"""
           trait ${Type.Name(inp.newtype)} extends $constructor {
             ..$forbidden
           }
           new ${Ctor.Name(inp.newtype)} {
             ..$members
           }
        """
      case Some(vr) =>
        val vrr = Pat.Var.Term(Term.Name(vr))
        q"""
          trait ${Type.Name(inp.newtype)} extends $constructor {
            ..$forbidden
          }
          val $vrr = new ${Ctor.Name(inp.newtype)} {
            ..$members
          }
        """
    }
  }
}
