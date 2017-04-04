package vista.semantics

import vista.Constants.forbiddenMethodBody
import vista.util.Equalities.defEquality
import vista.util.EqualitySet

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.contrib._

sealed trait Inst {
  val body: Defn.Class

  protected val db = vista.semantics.Database

  protected def members: Seq[Defn] =
    body.templ.collect[Defn] {
      case defn: Defn.Def => defn
      case valf: Defn.Val => valf
      case varf: Defn.Var => varf
    }

  def name: String = body.name.value

  def parents: Seq[String] = body.templ.parents.map(_.syntax.takeWhile(_ != '('))

  def methods: Set[Defn.Def] = {
    val parentMethods = parents.flatMap(db.get(_).methods)

    val memberDefns = members.collect {
      case d: Defn.Def if !d.mods.exists(_.is[Mod.Private]) => d
    }

    EqualitySet(parentMethods ++ memberDefns)
  }

  def visibilities: Set[Defn.Def] =
    methods.filterNot { d =>
      d.body isEqual forbiddenMethodBody
    }

  def forbidden: Set[Defn.Def] =
    methods.filter { d =>
      d.body isEqual forbiddenMethodBody
    }

  def generated: Boolean
  def notGenerated: Boolean = !generated
}

object Inst {
  case class Class(override val body: Defn.Class,
                   ctorMembers: Seq[Defn] = Seq.empty,
                   generated: Boolean = false)
      extends Inst {
    val ctor: Ctor.Primary = body.ctor

    override protected val members: Seq[Defn] = ctorMembers ++ super.members
  }

  case class Trait(tbody: Defn.Trait, generated: Boolean = false) extends Inst {
    override val body: Defn.Class =
      q"..${tbody.mods} class ${tbody.name}[..${tbody.tparams}] extends ${tbody.templ}"
  }
}
