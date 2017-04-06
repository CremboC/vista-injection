package vista.modifiers

import scala.meta._

object Subset {
  private val db = vista.semantics.Database

  def apply(term: Term): Lit.Boolean = {
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
        if (clazz.visibilities.contains(defn)) Lit.Boolean(true) else Lit.Boolean(false)
      case None => Lit.Boolean(false)
      case _ =>
        abort(
          s"A definition must be provided for the subset operator. Instead, ${f.stats} was given.")
    }
  }
}
