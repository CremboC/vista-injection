package vista.semantics

import scala.collection.mutable
import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object Database {
  type ClassName = String

  private class Store {
    private val store = mutable.Map.empty[ClassName, SClass]

    def add(clazz: SClass): Unit = {
      store.put(clazz.name, clazz)
    }

    def get(clazz: ClassName): Option[SClass] = store.get(clazz)

    def classes: Set[SClass] = store.values.to

    def clear(): Unit = {
      store.clear()
    }
  }

  private val database: Store = new Store

  def apply(value: ClassName): SClass = get(value)

  def addClass(c: Defn.Class): Unit = {
    database.add(new SClass(c))
  }

  def addClass(t: Defn.Trait, opResult: Boolean = false): Unit = {
    val c = q"..${t.mods} class ${t.name}[..${t.tparams}] extends ${t.templ}"
    database.add(new SClass(c, opResult))
  }

  def get(value: ClassName): SClass = {
    database.get(value).getOrElse {
      throw new IllegalStateException(s"Class <$value> not recorded in SemDB.")
    }
  }

  def classes: Set[SClass] = database.classes

  /**
    * Useful for testing purposes
    */
  def clear(): Unit = {
    database.clear()
  }
}
