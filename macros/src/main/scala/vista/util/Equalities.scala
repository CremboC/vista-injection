package vista.util

import org.scalactic.Equality
import vista.meta.xtensions.XDefn

import scala.meta._
import scala.meta.contrib._

/**
  * Created by Crembo on 2017-03-22.
  */
object Equalities {
  implicit val defnDefEquality: Equality[Defn.Def] =
    (a: Defn.Def, b: Any) =>
      b match {
        case b: Defn.Def => a.signature isEqual b.signature
        case _           => false
      }
}
