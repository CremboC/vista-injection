package vista.modifiers

import vista.semantics

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object Tratify {
  private implicit val db = semantics.Database

  def apply(cls: Defn.Class): Tree = {
    val q"..$mods class $name extends $template" = cls

    q"""
       ..$mods trait $name extends $template
      """
  }
}
