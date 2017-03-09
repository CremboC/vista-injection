package vista.modifiers

import vista.semantics

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object Tratify {
  def apply(cls: Defn.Class)(implicit db: semantics.Database.type): Tree = {
    val q"..$mods class $name extends $template" = cls

    q"""
       ..$mods trait $name extends $template
      """
  }
}
