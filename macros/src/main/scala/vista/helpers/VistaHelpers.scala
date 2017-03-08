package vista.helpers

import scala.meta._

/**
  * @author paulius
  */
object VistaHelpers {
  def isUnion(expr: Tree): Boolean = expr.syntax.contains("∪")
  def isForbid(expr: Tree): Boolean = expr.syntax.contains("∖")
  def isIntersect(expr: Tree): Boolean = expr.syntax.contains("∩")
  def isProduct(expr: Tree): Boolean = expr.syntax.contains("⨯")
}
