package vista.semantics

import scala.collection.immutable.Seq
import scala.meta._

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
    val parents       = body.templ.parents.map(_.syntax.takeWhile(_ != '('))
    val parentMethods = parents.flatMap(db.get(_).methods)

    (parentMethods ++ members.collect {
      case d: Defn.Def => d
    }).toSet
  }
}

object Inst {
  case class Class(override val body: Defn.Class, ctorMembers: Seq[Defn] = Seq.empty)
      extends Inst {
    val ctor: Ctor.Primary                    = body.ctor
    override protected val members: Seq[Defn] = ctorMembers ++ super.members
  }

  case class Trait(tbody: Defn.Trait) extends Inst {
    override val body: Defn.Class =
      q"..${tbody.mods} class ${tbody.name}[..${tbody.tparams}] extends ${tbody.templ}"
  }
}
