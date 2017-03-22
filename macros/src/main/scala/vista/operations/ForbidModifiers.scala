package vista.operations

import vista.helpers.OpHelpers
import vista.operations.expanders.ForbidOp.Forbid
import vista.operations.parsers.{OpOverload, OpVistas}

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object ForbidModifiers {
  val valOverloadModifier: PartialFunction[Tree, Term.Block] = {
    case defn: Defn.Val if OpHelpers.isForbid(defn) && OpHelpers.isOpOverload(defn) =>
      parseAndExpand[Defn.Val, OpOverload, Forbid](defn)
  }

  val valVistasModifier: PartialFunction[Tree, Term.Block] = {
    case defn: Defn.Val if OpHelpers.isForbid(defn) && OpHelpers.isOpVistas(defn) =>
      parseAndExpand[Defn.Val, OpVistas, Forbid](defn)
  }
}
