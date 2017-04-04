package vista.operations.expanders

import vista.operations.parsers.{OpInput, OpVistas}
import vista.semantics
import vista.semantics.Database.ClassName
import vista.util.Equalities.defEquality
import vista.util.EqualitySet
import vista.util.meta.xtensions.XDefn

import scala.collection.immutable.Seq
import scala.meta._

object UnionOp {
  type Union = Op[UnionOp.type]

  private val db = semantics.Database

  implicit object UnionCtor extends Constructable[Union] {
    override def members(input: OpInput): Seq[Defn] = input match {
      case input: OpVistas =>
        ctorMembersDefns(db(input.lclass), input.lvar) ++
          ctorMembersDefns(db(input.rclass), input.rvar)
      case _ => abort("Union is cannot be overridden")
    }
  }

  implicit object VistaExpander extends Expander[OpVistas, Union] {
    override def expand(inp: OpVistas): Defn.Trait = {
      val traitName = Type.Name(inp.newtype)

      val lclazz = db.get(inp.lclass)
      val rclazz = db.get(inp.rclass)

      val common = commonMethods(inp).to[Seq]

      /** Ensures we get the correct methods when we have
        * {{{
        *   trait A { def a; def b }
        *   trait Aa = forbid(def a)
        *   trait Ab = forbid(def b)
        *   trait UnionA = Aa union Ab
        * }}}
        * Without this, both a and b would be forbidden.
        */
      val methods = {
        def methodsFromSide(methods: Set[Defn.Def], className: ClassName): Set[Defn.Def] = {
          val map  = db.get(className).visibilities.map(m => m.signature.syntax -> m).toMap
          val spec = superDefBody(_: ClassName, _: Defn.Def, map)
          methods.map { m =>
            val body = spec(className, m)
            m.copy(mods = (m.mods :+ Mod.Override()).toSet.to, body = body)
          }
        }

        val commonBySignature = EqualitySet(common)
        val lmethods          = lclazz.visibilities.filterNot(commonBySignature.contains)
        val rmethods          = rclazz.visibilities.filterNot(commonBySignature.contains)
        methodsFromSide(lmethods, inp.lclass) ++ methodsFromSide(rmethods, inp.rclass)
      }

      val result = (common ++ methods).sortBy(_.name.syntax)

      q"""
        trait $traitName extends ${db.ctor(inp.lclass)} with ${db.ctor(inp.rclass)} {
          ..$result
        }
      """
    }
  }
}
