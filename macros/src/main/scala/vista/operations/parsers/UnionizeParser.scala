package vista.operations.parsers

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
private[operations] trait UnionizeParser extends Parser[UnionizeInput] {
  override def parseDefn(defn: Defn.Def): Option[UnionizeInput] = ???

  override def parseDefn(defn: Defn.Val): Option[UnionizeInput] = defn.decltpe match {
    case None => None
    case Some(typ) =>
      val (leftVar, leftType, rightVar, rightType) = defn.rhs.children match {
        case union :: left :: right :: Nil =>
          val q"$_[$leftType, $rightType]" = union
          val q"${leftVar: Term.Name}" = left
          val q"${rightVar: Term.Name}" = right

          (leftVar, leftType, rightVar, rightType)
        case _ => throw new RuntimeException("Illegal")
      }

      Some(UnionizeInput(leftType.syntax, rightType.syntax,
        leftVar.syntax, rightVar.syntax,
        typ.syntax, Some(defn.pats.head.syntax)))
  }
}

private[operations] case class UnionizeInput(
                                              lclass: String,
                                              rclass: String,
                                              lvar: String,
                                              rvar: String,
                                              newtype: String,
                                              newvar: Option[String] = None
                                            )