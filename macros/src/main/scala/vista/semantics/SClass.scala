package vista.semantics

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
class SClass(val body: Defn.Class) {
  private lazy val members: Seq[Defn] = body.templ.collect[Defn] {
    case defn: Defn.Def => defn
    case valf: Defn.Val => valf
  }

  lazy val methods: Seq[SMethod] = {
    members.collect {
      case Defn.Def(_, name, _, paramss, tpeopt, _) =>
        SMethod(name.value, SMethod.parseParams(paramss), tpeopt.get.syntax) // FIXME: get is bad
    }
  }

  lazy val vars: Seq[SVar] = {
    members.collect {
      case Defn.Val(_, names, tpeopt, _) =>
        SVar(names.head.toString(), tpeopt.get.syntax)
      case Defn.Var(_, names, tpeopt, _) =>
        SVar(names.head.toString(), tpeopt.get.syntax)
    }
  }
}

case class SMethod(name: String, params: Seq[SParam], returnType: String)

object SMethod {
  def parseParams(params: Seq[Seq[Term.Param]]): Seq[SParam] = {
    params.flatten.map {
      case Term.Param(_, name, tpeopt, _) => SParam(name.syntax, tpeopt.get.syntax)
    }
  }
}

case class SVar(name: String, typ: String)
case class SParam(name: String, typ: String)