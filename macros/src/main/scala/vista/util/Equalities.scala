package vista.util

import org.scalactic.Equality
import vista.util.meta.xtensions.XDefn

import scala.meta._
import scala.meta.contrib._

object Equalities {
  implicit val defEquality: Equality[Defn.Def] =
    (a: Defn.Def, b: Any) =>
      b match {
        case b: Defn.Def => a.signature isEqual b.signature
        case _           => false
    }
}
