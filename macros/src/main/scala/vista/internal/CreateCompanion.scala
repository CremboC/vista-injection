package vista.internal

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.contrib._

object CreateCompanion {
  def apply(clazz: Defn.Class, primary: Defn.Def, aux: Seq[Defn.Def]): Defn.Object = {
    q"object ${clazz.name.asTerm} { ..${primary +: aux} }"
  }
}
