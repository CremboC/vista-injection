package vista.internal

import vista.modifiers.Tratify
import vista.semantics.Inst

import scala.meta._
import scala.meta.contrib._

private[internal] object TransformClass {

  private val db = vista.semantics.Database

  /** 1. convert constructors into methods
    * 2. create companion with the methods
    * 3. convert companion's Term.New into correct form (instantiation of a trait)
    * 4. remove auxiliary constructors from source class
    * 5. convert source class into trait
    *
    * @return converted trait and its companion object
    */
  def apply(clazz: Defn.Class): (Defn.Trait, Defn.Object) = {
    val ctors = TranslateCtors(clazz)
    val companion = {
      val c = CreateCompanion(clazz, ctors._1, ctors._2)
      val templ = c.templ
        .transform {
          case t: Term.New => changeTermNew(t)
        }
        .asInstanceOf[Template]

      c.copy(templ = templ)
    }
    val clean = removeCtors(clazz)
    val treit = Tratify(clean)

    (treit, companion)
  }

  /** Convert a Term.New (instantiation) into a builder.
    */
  def convertCtor(term: Term.New): Term = {
    val ctor                  = term.templ.parents.head
    val q"$name(...$paramss)" = ctor

    val className = Term.Name(name.syntax)

    q"$className.$builderName(...$paramss)"
  }

  private def removeCtors(clazz: Defn.Class): Defn.Class = {
    val stats = clazz.templ.stats.map(_.filterNot(_.is[Ctor.Secondary]))
    clazz.copy(templ = clazz.templ.copy(stats = stats))
  }

  private def changeTermNew(term: Term.New): Term.New = {
    if (term.templ.stats.nonEmpty) term
    else {
      val ctor                  = term.templ.parents.head
      val q"$name(...$paramss)" = ctor

      val ctorMembers = db(name.syntax) match {
        case c: Inst.Class => c.ctorMembers
        case _             => Nil
      }

      def varName(pats: Seq[Pat]): Pat.Var.Term = Term.Name(pats.head.syntax).asPat
      def termName(pats: Seq[Pat]): Term.Name   = Term.Name(s"_${pats.head.syntax}")

      val overrides =
        ctorMembers.map {
          case vl: Defn.Val =>
            q"override val ${varName(vl.pats)} = ${termName(vl.pats)}"
          case vr: Defn.Var =>
            q"override var ${varName(vr.pats)} = ${termName(vr.pats)}"
        }

      q"new ${Ctor.Name(name.syntax)} { ..$overrides }"
    }
  }
}
