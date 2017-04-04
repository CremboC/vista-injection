package vista.modifiers

import scala.meta._

/**
  * Created by Crembo on 2017-03-30.
  */
object Subset {
  private val db = vista.semantics.Database

  def apply(term: Term): Lit = {
    val (f, typ, _) = term match {
      case q"${f: Term.Block}.⊆[$typ]($par)" => (f, typ, par)
      case q"${f: Term.Block} ⊆[$typ]($par)" => (f, typ, par)
      case q"${f: Term.Block} ⊆[$typ] $par"  => (f, typ, par)
      case _                                 => abort("Must be a subset of three formats")
    }

    val className =
      if (typ.syntax.contains(".")) typ.syntax.split("""\.""").last
      else typ.syntax

    db.getOption(className) match {
      case Some(clazz) if f.stats.head.is[Defn.Def] =>
        val defn = f.stats.head.asInstanceOf[Defn.Def]
        if (clazz.visibilities.contains(defn)) q"true" else q"false"
      case None => q"false"
    }
  }
}
