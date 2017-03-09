package vista.semantics

import scala.meta._

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

  lazy val methods: Seq[Defn.Def] = members.collect {
    case d: Defn.Def => d
  }

  lazy val vars: Seq[Either[Defn.Val, Defn.Var]] = members.collect {
    case va: Defn.Val => Left(va)
    case va: Defn.Var => Right(va)
  }
}

case class SMethod(name: String, types: Seq[String], params: Seq[Seq[String]])