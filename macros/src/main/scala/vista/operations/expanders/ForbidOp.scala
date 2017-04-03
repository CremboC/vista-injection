package vista.operations.expanders

import vista.Constants.forbiddenMethodBody
import vista.operations.parsers.{OpInput, OpOverload, OpVistas}
import vista.semantics
import vista.semantics.Database.ClassName
import vista.util.meta.xtensions._

import scala.collection.immutable.Seq
import scala.meta._

/**
  * Internal API of Forbid
  */
object ForbidOp {
  type Forbid = Op[ForbidOp.type]

  private val db = semantics.Database

  // FIXME: are you sure?
  implicit object ForbidCtor extends Constructable[Forbid] {
    override def members(input: OpInput): Seq[Defn] = input match {
      case _: OpVistas   => ctorMembersDefns(db(input.lclass), input.lvar)
      case _: OpOverload => ctorMembersDefns(db(input.lclass), input.lvar)
    }
  }

  implicit object VistaExpander extends Expander[OpVistas, Forbid] {
    override def expand(inp: OpVistas): Defn.Trait = {
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
          defnn.copy(mods = (defn.mods :+ Mod.Override()).toSet.to, body = forbiddenMethodBody)
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

      val result = (forbiddenDefns ++ allowedDefns).to[Seq].sortBy(_.name.syntax)

      val lctor = db.ctor(inp.lclass)
      val rctor = db.ctor(inp.rclass)

      q"""
        trait ${Type.Name(inp.newtype)} extends $lctor with $rctor {
          ..$result
        }
      """
    }
  }

  implicit object OverloadExpander extends Expander[OpOverload, Forbid] {
    override def expand(inp: OpOverload): Defn.Trait = {
      val forbidden = inp.methods
        .map { defn =>
          defn.copy(mods = (defn.mods :+ Mod.Override()).toSet.to, body = forbiddenMethodBody)
        }
        .to[Seq]

      val constructor = db.ctor(inp.lclass)

      q"""
        trait ${Type.Name(inp.newtype)} extends $constructor {
          ..$forbidden
        }
      """
    }
  }
}
