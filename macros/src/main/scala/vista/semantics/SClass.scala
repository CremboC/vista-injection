package vista.semantics

import scala.collection.immutable.Seq
import scala.meta._

/**
  * @author Paulius Imbrasas
  */
class SClass(val body: Defn.Class, val opResult: Boolean = false) {
  private val db = vista.semantics.Database

  private lazy val members: Seq[Defn] = body.templ.collect[Defn] {
    case defn: Defn.Def => defn
    case valf: Defn.Val => valf
    case varf: Defn.Var => varf
  }

  val name: String = body.name.value

  def methods: Set[Defn.Def] = {
    val parents       = body.templ.parents.map(_.syntax.takeWhile(_ != '('))
    val parentMethods = parents.flatMap(db.get(_).methods)

    (parentMethods ++ members.collect {
      case d: Defn.Def => d
    }).toSet
  }

  lazy val vars: Seq[Either[Defn.Val, Defn.Var]] = members.collect {
    case va: Defn.Val => Left(va)
    case va: Defn.Var => Right(va)
  }
}

case class SMethod(name: String, types: Seq[String], params: Seq[Seq[String]])
