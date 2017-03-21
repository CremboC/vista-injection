package vista.semantics

import _root_.meta.xtensions.XDefn
import org.scalactic.Equality
import util.EqualitySet

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.contrib._

sealed trait Inst {
  protected val db = vista.semantics.Database
  val body: Defn.Class

  protected def members: Seq[Defn] = body.templ.collect[Defn] {
    case defn: Defn.Def => defn
    case valf: Defn.Val => valf
    case varf: Defn.Var => varf
  }

  lazy val name: String = body.name.value

  def methods: Set[Defn.Def] = {
    val parents = body.templ.parents.map(_.syntax.takeWhile(_ != '('))
    val parentMethods = parents.flatMap(db.get(_).methods)

    val memberDefns = members.collect {
      case d: Defn.Def => d
    }

    EqualitySet(parentMethods ++ memberDefns)
  }
}

object Inst {
  implicit val defnDefEquality: Equality[Defn.Def] =
    (a: Defn.Def, b: Any) => b match {
      case b: Defn.Def => a.signature isEqual b.signature
      case _ => false
    }

  case class Class(override val body: Defn.Class, ctorMembers: Seq[Defn] = Seq.empty)
    extends Inst {
    val ctor: Ctor.Primary = body.ctor
    override protected val members: Seq[Defn] = ctorMembers ++ super.members
  }

  case class Trait(tbody: Defn.Trait) extends Inst {
    override val body: Defn.Class =
      q"..${tbody.mods} class ${tbody.name}[..${tbody.tparams}] extends ${tbody.templ}"
  }

}
