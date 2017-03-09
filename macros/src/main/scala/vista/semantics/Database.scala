package vista.semantics

import scala.collection.mutable
import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object Database {


  private val database: mutable.Set[SClass] = mutable.Set.empty

  def addClass(c: Defn.Class): Boolean = {
    database.add(new SClass(c))
  }

  def get(value: String): SClass = database.find(_.name == value).getOrElse {
    throw new IllegalStateException(s"Class <$value> not recorded in SemDB.")
  }

  def classes: Set[SClass] = database.toSet
}
