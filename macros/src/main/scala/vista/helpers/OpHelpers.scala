package vista.helpers

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object OpHelpers {
  def isUnion(expr: Tree): Boolean     = expr.syntax.contains("∪")
  def isForbid(expr: Tree): Boolean    = expr.syntax.contains("∖")
  def isIntersect(expr: Tree): Boolean = expr.syntax.contains("∩")
  def isProduct(expr: Tree): Boolean   = expr.syntax.contains("⨯")

  def hasOp(expr: Tree): Boolean =
    isUnion(expr) || isForbid(expr) || isIntersect(expr) || isProduct(expr)
}
