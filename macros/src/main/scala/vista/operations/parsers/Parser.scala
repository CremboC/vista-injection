package vista.operations.parsers

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
trait Parser[T] {
  def parseDefn(defn: Defn.Val): Option[T]
  def parseDefn(defn: Defn.Def): Option[T]
}
