package vista.semantics

import vista.Constants.forbiddenMethodBody
import vista.operations.ResultOf
import vista.operations.expanders.{Op, ProductOp}
import vista.util.Equalities.defEquality
import vista.util.EqualitySet

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.contrib._

sealed trait Inst {
  val body: Defn.Class

  protected val db = vista.semantics.Database

  def members: Seq[Defn] = body.templ.extract[Defn].to

  def membersWithParents: Seq[Defn] = members ++ parents.flatMap(db.get(_).membersWithParents)

  def name: String = body.name.value

  def parents: Seq[String] = body.templ.parents.map(_.syntax.takeWhile(_ != '('))

  def methods: EqualitySet[Defn.Def] = {
    val parentMethods = parents.flatMap(db.get(_).methods)

    val memberDefns = members.collect {
      case d: Defn.Def if !d.mods.exists(_.is[Mod.Private]) => d
    }

    if (isResultOf[ProductOp.Product]) EqualitySet(memberDefns)
    else EqualitySet(parentMethods ++ memberDefns)
  }

  def visibilities: EqualitySet[Defn.Def] =
    methods.filterNot { d =>
      d.body isEqual forbiddenMethodBody
    }

  def forbidden: EqualitySet[Defn.Def] =
    methods.filter { d =>
      d.body isEqual forbiddenMethodBody
    }

  def isGenerated: Boolean
  def nonGenerated: Boolean = !isGenerated

  def isResultOf[A <: Op[_]: ResultOf]: Boolean = ResultOf[A].check(this)
}

object Inst {
  case class Class(override val body: Defn.Class,
                   ctorMembers: Seq[Defn] = Seq.empty,
                   isGenerated: Boolean = false)
      extends Inst {
    val ctor: Ctor.Primary = body.ctor

    override def members: Seq[Defn] = ctorMembers ++ super.members
  }

  case class Trait(tbody: Defn.Trait, isGenerated: Boolean = false) extends Inst {
    override val body: Defn.Class =
      q"..${tbody.mods} class ${tbody.name}[..${tbody.tparams}] extends ${tbody.templ}"
  }
}
