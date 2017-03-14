package vista.semantics

import scala.collection.mutable
import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object Database {
  type ClassName = String

  private val database: mutable.Set[SClass] = mutable.Set.empty

  def apply(value: ClassName): SClass = get(value)

  def addClass(c: Defn.Class): Boolean = {
    database.add(new SClass(c))
  }

  def addClass(t: Defn.Trait): Boolean = {
    val c = q"..${t.mods} class ${t.name}[..${t.tparams}] extends ${t.templ}"
    database.add(new SClass(c))
  }

  def get(value: ClassName): SClass = database.find(_.name == value).getOrElse {
    throw new IllegalStateException(s"Class <$value> not recorded in SemDB.")
  }

  def classes: Set[SClass] = database.toSet
}
