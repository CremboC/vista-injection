package vista.operations

import vista.helpers.OpHelpers
import vista.operations.expanders.ForbidOp.Forbid
import vista.operations.parsers.OpOverload

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object ForbidModifiers {
  val defnValModifier: PartialFunction[Tree, Term.Block] = {
    case defn: Defn.Val if OpHelpers.isForbid(defn) =>
      parseAndExpand[Defn.Val, OpOverload, Forbid](defn)
  }
}
