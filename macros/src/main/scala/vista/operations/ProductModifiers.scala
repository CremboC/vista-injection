package vista.operations

import vista.helpers.OpHelpers
import vista.operations.expanders.ProductOp.Product
import vista.operations.parsers.OpVistas

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object ProductModifiers {
  val defnValModifier: PartialFunction[Tree, Term.Block] = {
    case defn: Defn.Val if OpHelpers.isProduct(defn) =>
      parseAndExpand[Defn.Val, OpVistas, Product](defn)
  }
}
