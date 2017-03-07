package vista.helpers

import scala.meta._

/**
  * @author paulius
  */
object VistaHelpers {
  def isUnion(expr: Term): Boolean = expr.syntax.contains("∪")
  def isForbid(expr: Term): Boolean = expr.syntax.contains("∖")
}
