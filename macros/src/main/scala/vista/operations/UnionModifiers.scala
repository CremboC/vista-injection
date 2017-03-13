package vista.operations

import vista.helpers.OpHelpers
import vista.operations.expanders.UnionOp.Union
import vista.operations.parsers.OpVistas

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object UnionModifiers {
  val defnValModifier: PartialFunction[Tree, Term.Block] = {
    case defn: Defn.Val if OpHelpers.isUnion(defn) => parseAndExpand[Defn.Val, OpVistas, Union](defn)
  }
}
