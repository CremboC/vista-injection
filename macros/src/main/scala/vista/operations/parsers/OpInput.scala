package vista.operations.parsers

import scala.meta.Defn

/**
  * @author Paulius Imbrasas
  */


sealed trait OpInput {
  def lclass: String
  def lvar: String
  def newtype: String
  def newvar: Option[String] = None
}

case class OpVistas(
                   lclass: String,
                   rclass: String,
                   lvar: String,
                   rvar: String,
                   newtype: String,
                   override val newvar: Option[String] = None
                 ) extends OpInput

case class OpOverload(
                       lclass: String,
                       lvar: String,
                       newtype: String,
                       methods: Seq[Defn.Def],
                       override val newvar: Option[String] = None
                     ) extends OpInput