package vista.semantics

import scala.collection.immutable.Seq
import scala.meta._
import meta.XDefn

/**
  * @author Paulius Imbrasas
  */
class SClass(val body: Defn.Class) {
  private lazy val members: Seq[Defn] = body.templ.collect[Defn] {
    case defn: Defn.Def => defn
    case valf: Defn.Val => valf
    case varf: Defn.Var => varf
  }

  val name: String = body.name.value

  def methods(implicit db: vista.semantics.Database.type): Set[Defn.Def] = {
    val parents = body.templ.parents.map(_.syntax)
    val parentMethods = parents.flatMap(db.get(_).methods)

    //    val signatures = parentMethods.map(_.signature) // signatures

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