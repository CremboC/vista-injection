package vista.operations

import vista.helpers.OpHelpers
import vista.operations.expanders.IntersectOp.Intersect
import vista.operations.parsers.OpVistas

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object IntersectModifiers {
  val defnValModifier: PartialFunction[Tree, Term.Block] = {
    case defn: Defn.Val if OpHelpers.isIntersect(defn) =>
      parseAndExpand[Defn.Val, OpVistas, Intersect](defn)
  }
}
