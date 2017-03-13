package vista.operations.parsers

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
private[operations] object ProductParser {
  implicit val defnValToInput: Parser[Defn.Val, ProductInput] = (defn: Defn.Val) => ???
}

private[operations] case class ProductInput(
                                              lclass: String,
                                              rclass: String,
                                              lvar: String,
                                              rvar: String,
                                              newtype: String,
                                              newvar: Option[String] = None
                                            )