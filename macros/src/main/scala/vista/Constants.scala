package vista

import scala.meta._

private[vista] object Constants {
  final val VistaTypeR = """Vista\[(.+)\]"""

  final val GenName = "gen$vista"
  final val AnyV    = "vista.lib.AnyV"

  final val Forbid    = "∖"
  final val Intersect = "∩"
  final val Union     = "∪"
  final val Product   = "⨯"
  final val Subset    = "⊆"

  final val forbiddenMethodBody = q"throw new vista.lib.ForbiddenMethodException"
}
