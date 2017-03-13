package vista.operations.internal

import vista.operations.parsers.ProductInput
import vista.semantics

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
private[operations] object ProductImpl {
  def apply(inp: ProductInput)(implicit db: semantics.Database.type): Term.Block = ???
}
