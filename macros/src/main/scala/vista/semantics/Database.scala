package vista.semantics

import vista.Constants
import vista.modifiers.Tratify

import scala.collection.mutable
import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object Database {

  type ClassName = String

  private class Store {
    private val store = mutable.Map.empty[ClassName, Inst]

    def add(clazz: Inst): Unit = {
      store.put(clazz.name, clazz)
    }

    def get(clazz: ClassName): Option[Inst] = store.get(clazz)

    def classes: Set[Inst] = store.values.to

    def clear(): Unit = {
      store.clear()
    }
  }

  private val store: Store = new Store

  def apply(value: ClassName): Inst = get(value)

  def add(c: Defn.Class): Unit = add(c, generated = false)

  // FIXME: add "generated" parameter
  def add(c: Defn.Class, generated: Boolean): Unit = {
    import vista.meta.xtensions.{XDeclVal, XDeclVar}

    val decls = Tratify.ctorToDecls(c).map {
      case d: Decl.Val => d.asDefnVal
      case d: Decl.Var => d.asDefnVar
    }
    store.add(Inst.Class(c, decls, generated))
  }

  def add(c: Defn.Trait): Unit = add(c, generated = false)

  def add(c: Defn.Trait, generated: Boolean): Unit = {
    if (!exists(c.name.value))
      store.add(Inst.Trait(c, generated))
  }

  def get(value: ClassName): Inst = {
    store.get(value).getOrElse {
      throw new IllegalStateException(s"Class <$value> not recorded in SemDB.")
    }
  }

  def exists(name: ClassName): Boolean = store.get(name).isDefined

  def ctor(name: ClassName): Ctor.Name =
    if (this(name).generated) Ctor.Name(s"${Constants.GenName}.$name")
    else Ctor.Name(name)

  def classes: Set[Inst] = store.classes

  /**
    * Useful for testing purposes
    */
  def clear(): Unit = {
    store.clear()
  }
}
