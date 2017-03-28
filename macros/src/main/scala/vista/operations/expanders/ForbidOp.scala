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

  implicit object VistaExpander extends Expander[OpVistas, Forbid] {
    override def expand(inp: OpVistas): Term.Block = {
      val lclazz = db(inp.lclass)
      val rclazz = db(inp.rclass)

      val lmethods = lclazz.visibilities
      val rmethods = rclazz.visibilities

      val allowed    = lmethods.signatures \ rmethods.signatures
      val disallowed = (lmethods ++ rmethods).signatures \ allowed

      def visibilitiesMap(className: ClassName): Map[ClassName, Defn.Def] =
        db.get(className).visibilities.map(m => m.signature.syntax -> m).toMap

      val forbiddenDefns = {
        val map = visibilitiesMap(inp.lclass) ++ visibilitiesMap(inp.rclass)

        disallowed.map { defn =>
          val defnn = map(defn.syntax)
          defnn.copy(mods = (defn.mods :+ Mod.Override()).toSet.to,
                     body = q"throw new NoSuchMethodException")
        }
      }

      val allowedDefns = {
        val map  = visibilitiesMap(inp.lclass)
        val spec = superDefBody(inp.lclass, _: Defn.Def, map)
        allowed.map { defn =>
          val body = spec(defn)
          map(defn.signature.syntax)
            .copy(mods = (defn.mods :+ Mod.Override()).toSet.to, body = body)
        }
      }

      val members = ctorMembersDefns(lclazz, inp.lvar)

      val result = (forbiddenDefns ++ allowedDefns).to[Seq].sortBy(_.name.syntax)

      inp.newvar match {
        case None => ???
        case Some(nvar) =>
          q"""
          trait ${Type.Name(inp.newtype)} extends ${Ctor.Name(inp.lclass)} with ${Ctor.Name(
            inp.rclass)} {
            ..$result
          }
          val ${Term.Name(nvar).asPat} = new ${Ctor.Name(inp.newtype)} {
            ..$members
          }
        """
      }
    }
  }

  implicit object OverloadExpander extends Expander[OpOverload, Forbid] {
    override def expand(inp: OpOverload): Term.Block = {
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
}
