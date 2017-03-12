package vista.operations.parsers

import scala.collection.immutable.Seq
import scala.meta._

/**
  * @author Paulius Imbrasas
  */
private[operations] trait ForbidParser extends Parser[ForbidInput] {

  override def parseDefn(defn: Defn.Def): Option[ForbidInput] = defn.decltpe match {
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

  override def parseDefn(defn: Defn.Val): Option[ForbidInput] = defn.decltpe match {
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

      val Defn.Val(_, param, _, _) = defn
      val paramname = param.head

      Some(ForbidInput(typ.syntax, subjectType.syntax, methods, Some(paramname.syntax)))
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