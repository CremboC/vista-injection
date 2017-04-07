package vista.util.meta

import vista.util.{Counter, EqualitySet}

import scala.collection.immutable.Seq
import scala.meta._

trait XDefn {

  /**
    * Defn.Def extensions
    */
  implicit class XDefn(defn: Defn.Def) {
    def signature(implicit counter: Counter[Defn.Def] = new Counter()): Defn.Def = {
      // ensures that def g() === def g
      val name = "p"

      val paramss =
        if (defn.paramss.isEmpty) Seq(Seq.empty)
        else
          defn.paramss.map(_.map { p =>
            p.copy(name = Term.Name(s"$name${counter.next}"))
          })

      defn.copy(mods = Seq.empty, body = Term.Block(Nil), decltpe = None, paramss = paramss)
    }

    /**
      * A normalised definition is one without mods and no decltpe.
      * This allows to compare the signature and the body of defns
      *
      * Useful for comparing
      * {{{
      *   override def a(b: Int): String = "hello"
      *   ---- and ----
      *   def a(b: Int): Int = b
      * }}}
      * which are, by signature, the same, but their body is different
      *
      * Note it is impossible to say whether the bodies are equivalent as it
      * requires solving the halting problem by evaluating it
      */
    def normalize: Defn.Def = defn.copy(mods = Seq.empty, decltpe = None)

    def hasMultiParamList: Boolean = {
      defn.paramss.size > 1
    }

    def hasParenthesis: Boolean = defn.paramss.nonEmpty
  }

  implicit class XDefnIterable[A <: Defn.Def](self: Iterable[A]) {
    import vista.util.Equalities.defEquality

    @inline
    def signatures: Set[Defn.Def] = EqualitySet(self.map(_.signature))

    @inline
    def normalized: Set[Defn.Def] = EqualitySet(self.map(_.normalize))
  }
}
