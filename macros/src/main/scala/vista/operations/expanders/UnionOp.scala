package vista.operations.expanders

import vista.meta.xtensions.XDefn
import vista.operations.parsers.OpVistas
import vista.semantics
import vista.semantics.Database.ClassName
import vista.util.Equalities.defEquality
import vista.util.EqualitySet

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.contrib._

/**
  * Internal API implementation
  */
private[operations] object UnionOp {
  type Union = Op[UnionOp.type]

  private val db = semantics.Database

  val expander: Expander[OpVistas, Union] = (inp: OpVistas) => {
    val traitName = Type.Name(inp.newtype)

    val lclazz = db.get(inp.lclass)
    val rclazz = db.get(inp.rclass)

    val common = commonMethods(inp).to[Seq]

    val methods = {
      def methodsFromSide(methods: Set[Defn.Def], className: ClassName): Set[Defn.Def] = {
        val map = db.get(className).visibilities.map(m => m.signature.syntax -> m).toMap
        val spec = superDefBody(_: ClassName, _: Defn.Def, map)
        methods.map { m =>
          val body = spec(className, m)
          m.copy(mods = (m.mods :+ Mod.Override()).toSet.to, body = body)
        }
      }

      val commonBySignature = EqualitySet(common)
      val lmethods = lclazz.visibilities.filterNot(commonBySignature.contains)
      val rmethods = rclazz.visibilities.filterNot(commonBySignature.contains)
      methodsFromSide(lmethods, inp.lclass) ++ methodsFromSide(rmethods, inp.rclass)
    }

    val result = (common ++ methods).sortBy(_.name.syntax)

    val members = ctorMembersDefns(lclazz, inp.lvar) ++ ctorMembersDefns(rclazz, inp.rvar)

    inp.newvar match {
      case None =>
        q"""
            trait $traitName extends ${Ctor.Name(inp.lclass)} with ${Ctor.Name(inp.rclass)} {
              ..$result
            }
            new ${Ctor.Name(traitName.value)} {
              ..$members
            }
        """

      case Some(nvar) =>
        q"""
            trait $traitName extends ${Ctor.Name(inp.lclass)} with ${Ctor.Name(inp.rclass)} {
              ..$result
            }
            val ${Term.Name(nvar).asPat} = new ${traitName.asCtorRef} {
              ..$members
            }
        """
    }
  }
}
