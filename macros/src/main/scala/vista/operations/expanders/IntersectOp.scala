package vista.operations.expanders

import vista.meta.xtensions._
import vista.operations.parsers.{OpInput, OpOverload, OpVistas}
import vista.semantics

import scala.collection.immutable.Seq
import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object IntersectOp {
  type Intersect = Op[IntersectOp.type]

  private val db = semantics.Database

  implicit object IntersectCtor extends Constructable[Intersect] {
    override def members(input: OpInput): Seq[Defn] = input match {
      case input: OpVistas =>
        ctorMembersDefns(db(input.lclass), input.lvar) ++
          ctorMembersDefns(db(input.rclass), input.rvar)
      case input: OpOverload => ctorMembersDefns(db(input.lclass), input.lvar)
    }
  }

  implicit object VistaExpander extends Expander[OpVistas, Intersect] {
    override def expand(inp: OpVistas): Defn.Trait = {
      val traitName = Type.Name(inp.newtype)

      val leftTypeCtor  = db.ctor(inp.lclass)
      val rightTypeCtor = db.ctor(inp.rclass)

      val lclazz = db(inp.lclass)
      val rclazz = db(inp.rclass)

      val lsignatures = lclazz.visibilities.signatures
      val rsignatures = rclazz.visibilities.signatures

      // we're only overriding the methods that are no longer allowed
      // mintersect will return a list of methods which are allowed, but
      // we can only disallow methods, hence that is used here
      val disjointMethods = lsignatures <-> rsignatures
      val forbidden = disjointMethods.map { m =>
        m.copy(body = q"throw new NoSuchMethodException", mods = m.mods :+ Mod.Override())
      }

      // get common signatures in order to avoid "trait X inherits conflicting members"
      val common = commonMethods(inp)
      val result = (forbidden ++ common).to[Seq].sortBy(_.name.syntax)

      q"""
         trait $traitName extends $leftTypeCtor with $rightTypeCtor {
           ..$result
         }
      """
    }
  }
}
