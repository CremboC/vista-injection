package vista.operations.parsers

import scala.collection.immutable.Seq
import scala.meta._


private[operations] object ForbidParser {
  implicit val defnValToInput: Parser[Defn.Val, ForbidInput] =
    (defn: Defn.Val) => defn.decltpe match {
      case None => None
      case Some(typ) =>
        val q"$_[..$typargs](..$args)" = defn.rhs
        val subjectType = typargs.head

        val methods = {
          val q"..$stats" = args.last
          stats.collect {
            case d: Defn.Def => d
          }
        }

        val paramname = defn.pats.head
        Some(ForbidInput(typ.syntax, subjectType.syntax, methods, Some(paramname.syntax)))
    }

  implicit val defnDefToInput: Parser[Defn.Def, ForbidInput] =
    (defn: Defn.Def) => defn.decltpe match {
      case None => None
      case Some(typ) =>
        val q"$_[..$typargs](..$args)" = defn.body
        val subjectType = typargs.head

        val methods = {
          val q"..$stats" = args.last
          stats.collect {
            case d: Defn.Def => d
          }
        }

        Some(ForbidInput(typ.syntax, subjectType.syntax, methods))
    }

}

private[operations] case class ForbidInput(
                                            nclass: String,
                                            oclass: String,
                                            methods: Seq[Defn.Def],
                                            varname: Option[String] = None,
                                            lsource: Option[String] = None,
                                            rsource: Option[String] = None
                                          )