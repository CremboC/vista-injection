package vista.operations

import vista.meta.xtensions._
import vista.operations.parsers.{OpInput, OpVistas}
import vista.semantics.Database.ClassName
import vista.semantics.Inst

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.contrib._

/**
  * @author Paulius Imbrasas
  */
package object expanders {

  sealed trait Op[A]

  trait Expander[A <: OpInput, B <: Op[_]] {
    def expand(input: A): Defn.Trait
  }

  object Expander {
    def apply[A <: OpInput, B <: Op[_]](implicit expander: Expander[A, B]): Expander[A, B] =
      expander
  }

  trait Constructable[A <: Op[_]] {
    def members(input: OpInput): Seq[Defn]
  }

  object Constructable {
    def apply[A <: Op[_]: Constructable]: Constructable[A] = implicitly
  }

  /**
    * Merges common methods, the correct one is executed
    *
    * This is needed due to:
    * {{{
    *   trait A {
    *     def a: Int
    *   }
    *
    *   trait B {
    *     def a: Int
    *   }
    *
    *   trait AB extends A with B {
    *     override def a: Int = super[A].a
    *   }
    * }}}
    * When inhering a common method, its source must be declared
    *
    * @param inp
    * @return
    */
  def commonMethods(inp: OpVistas): Set[Defn.Def] = {
    val db = vista.semantics.Database

    def visibilitiesMap(className: ClassName): Map[ClassName, Defn.Def] =
      db.get(className).visibilities.map(m => m.signature.syntax -> m).toMap

    val map = visibilitiesMap(inp.rclass) ++ visibilitiesMap(inp.lclass)

    val lvisibilities = db(inp.lclass).visibilities.signatures
    val rvisibilities = db(inp.rclass).visibilities.signatures

    // specialised version of superDefBody
    val spec = superDefBody(_: ClassName, _: Defn.Def, map)

    lvisibilities mintersect rvisibilities map { m =>
      val body = spec(inp.lclass, m)
      map(m.signature.syntax).copy(mods = (m.mods :+ Mod.Override()).toSet.to, body = body)
    }
  }

  def superDefBody(className: ClassName, mn: Defn.Def, map: Map[ClassName, Defn.Def]): Term = {
    val m = map(mn.signature.syntax)

    val tparams = m.tparams.map(typ => targ"${typ.name.asType}")
    val paramss = m.paramss.map(_.map(arg => arg"${arg.name.asTerm}"))

    // FIXME: there must be a better way...
    if (tparams.nonEmpty && paramss.nonEmpty) {
      q"super[${Type.Name(className)}].${m.name}[..$tparams](...$paramss)"
    } else if (tparams.nonEmpty) {
      q"super[${Type.Name(className)}].${m.name}[..$tparams]"
    } else if (paramss.nonEmpty) {
      q"super[${Type.Name(className)}].${m.name}(...$paramss)"
    } else {
      q"super[${Type.Name(className)}].${m.name}"
    }
  }

  def ctorMembersDefns(inst: Inst, varname: String): Seq[Defn] =
    inst match {
      case cls: Inst.Class =>
        cls.ctorMembers.zip(cls.ctor.paramss.flatten).map {
          case (member, arg) =>
            val value = s"$varname.${arg.name.value}".parse[Term].get
            member match {
              case m: Defn.Val => m.copy(mods = (m.mods :+ Mod.Override()).toSet.to, rhs = value)
              case m: Defn.Var =>
                m.copy(mods = (m.mods :+ Mod.Override()).toSet.to, rhs = Some(value))
            }
        }
      case _ => Seq.empty
    }
}
