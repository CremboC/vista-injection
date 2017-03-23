package vista.helpers

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object OpHelpers {
  private final val opVistasR = """.\[(.*), (.*)\]""".r
  private final val overloadR = """.\[(.*)]""".r

  def isUnion(expr: Tree): Boolean     = expr.syntax.contains("∪")
  def isForbid(expr: Tree): Boolean    = expr.syntax.contains("∖")
  def isIntersect(expr: Tree): Boolean = expr.syntax.contains("∩")
  def isProduct(expr: Tree): Boolean   = expr.syntax.contains("⨯")

  def isOpVistas(tree: Tree): Boolean =
    tree match {
      case d: Defn.Val => opVistasR.findFirstMatchIn(d.syntax).isDefined
      case _           => false
    }

  def isOpOverload(tree: Tree): Boolean =
    tree match {
      case d: Defn.Val => overloadR.findFirstMatchIn(d.syntax).isDefined
      case _           => false
    }

  def hasOp(expr: Tree): Boolean =
    isUnion(expr) || isForbid(expr) || isIntersect(expr) || isProduct(expr)

  def hasCaseMod(cls: Defn.Class): Boolean = cls.mods.exists(_.is[Mod.Case])
}
