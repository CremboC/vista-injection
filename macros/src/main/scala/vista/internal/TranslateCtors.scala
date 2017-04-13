package vista.internal

import vista.util.meta.xtensions._

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.contrib._

private[internal] object TranslateCtors {

  // 3. for main constructor:
  //    def apply(_arg: ?*): T = new T { override ... }
  // 4. for auxiliary
  //    def apply(<copy of aux>): T = {
  //      val self = s/self/this(...)
  //      ...$stats
  //      self
  //    }

  def apply(defn: Defn.Class): (Defn.Def, Seq[Defn.Def]) = {
    val primaryCtor = defn.ctor
    val auxs        = defn.templ.collect { case c: Ctor.Secondary => c }

    val converter = new Converter(defn)
    (converter.primary(primaryCtor), auxs.map(converter.auxiliary))
  }

  private class Converter(clazz: Defn.Class) {

    def primary(ctor: Ctor.Primary): Defn.Def = {
      val paramss = ctor.paramss.map(_.map(p =>
        p.copy(name = Term.Name(s"_${p.name.value}"), mods = Seq.empty)))
      val body = q"new ${clazz.name.asCtorRef}(...${paramss.asTermArg})"

      Defn.Def(mods = Seq.empty,
               name = builderName,
               tparams = clazz.tparams,
               paramss = if (paramss.isEmpty) Seq(Seq.empty) else paramss,
               decltpe = None,
               body = body)
    }

    def auxiliary(ctor: Ctor.Secondary): Defn.Def = {
      val paramss = ctor.paramss.asTermArg

      val (primaryCtor, rest) =
        if (ctor.body.is[Term.Apply]) { // if is def this() = this()
          (ctor.body.asInstanceOf[Term.Apply], Nil)
        } else { // else is term.block
          val (head :: rest) = ctor.body.asInstanceOf[Term.Block].stats
          (head.asInstanceOf[Term.Apply], rest)
        }

      val term = s"$builderName${primaryCtor.syntax.stripPrefix("this")}".parse[Term].get

      val first = q"val ${selfName.asPat} = $term"
      val last  = q"$selfName"
      val stats = first +: rest :+ last

      val body = Term.Block(stats)
      Defn.Def(mods = Seq.empty,
               name = builderName,
               tparams = clazz.tparams,
               paramss = ctor.paramss,
               decltpe = Option(clazz.name.asType),
               body = body)
    }
  }
}
