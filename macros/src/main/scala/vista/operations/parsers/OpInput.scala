package vista.operations.parsers

import scala.meta.Defn

/**
  * @author Paulius Imbrasas
  */
sealed trait OpInput {
  def lclass: String
  def lvar: String
  def newtype: String
}

case class OpVistas(
    lclass: String,
    rclass: String,
    lvar: String,
    rvar: String,
    newtype: String
) extends OpInput

case class OpOverload(
    lclass: String,
    lvar: String,
    newtype: String,
    methods: Seq[Defn.Def]
) extends OpInput
