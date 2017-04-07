package vista.internal

import vista.helpers.OpHelpers
import vista.modifiers.Tratify
import vista.operations.expanders._
import vista.operations.parsers.{OpOverload, OpVistas, Parser}
import vista.semantics.Inst
import vista.{Constants, semantics}

import scala.meta.Term.Block
import scala.meta._

private[vista] object Enable {
  private val db = semantics.Database

  def apply(obj: Defn.Object): Defn.Object = {
    obj.copy(templ = expand(obj.templ))
  }

  def apply(clazz: Defn.Class): Defn.Class = {
    clazz.copy(templ = expand(clazz.templ))
  }

  private def expand(templ: Template): Template = {
    def classIsRecorded(term: Term.New): Boolean =
      if (term.templ.parents.isEmpty) false
      else {
        val className = term.templ.parents.head.syntax.takeWhile(_ != '(').trim
        db.exists(className) && db.get(className).isInstanceOf[Inst.Class]
      }

    val stats = templ.stats.getOrElse(abort("Cannot enable vistas on an empty object"))

    // build up SemDB
    buildDatabase(templ)

    // build constructors for all classes
    val transformed = templ
      .collect {
        case d: Defn.Class => d
      }
      .filterNot(c => OpHelpers.hasCaseMod(c))
      .map(c => TransformClass(c))

    val classMap = transformed.map(_._1).map(t => t.name.value -> t).toMap

    // expand operators into traits
    val traits = vista.internal.ExpandOp(templ)

    // generator wrapper object with all traits in it
    val generatedWrapper = q"object ${Term.Name(Constants.GenName)} { ..$traits }"

    val Block(nstats) = Block(stats)
      .transform {
        case classdefn: Defn.Class if !OpHelpers.hasCaseMod(classdefn) =>
          classMap(classdefn.name.value)

        case term: Term.New if classIsRecorded(term) =>
          val className = term.templ.parents.head.syntax.takeWhile(_ != '(').trim

          if (db(className).nonGenerated) TransformClass.convertCtor(term)
          else Tratify(term)

        case OpHelpers.HasOp(t) =>
          val parser = t match {
            case OpHelpers.OpVistas()   => Parser[Term.Apply, OpVistas]
            case OpHelpers.OpOverload() => Parser[Term.Apply, OpOverload]
          }

          val op = parser.parse(t)

          val ctorable = t match {
            case OpHelpers.Forbid(_)    => Constructable[ForbidOp.Forbid]
            case OpHelpers.Union(_)     => Constructable[UnionOp.Union]
            case OpHelpers.Intersect(_) => Constructable[IntersectOp.Intersect]
            case OpHelpers.Product(_)   => Constructable[ProductOp.Product]
          }

          val members = ctorable.members(op)
          q"new ${db.ctor(op.newtype)} { ..$members }"
      }
      .transform {
        case s @ q"$t.invoke[$typ](..$funs)(..$args)" if db.exists(typ.syntax) =>
          // FIXME: this is really bad, I am truly sorry
          val reduce = funs.map(_.syntax).reduce(_ + _)
          val argss = args
            .map(_.syntax)
            .reduce(_ + _)
            .replace("Seq", "")
            .replace("Nil", "()")
            .replace("((", "(")
            .replace("))", ")")
            .replace(", ", "")

          s"${t.syntax}.$reduce$argss".parse[Term].get
      }

    val unparsed = nstats.find {
      case s: Stat if OpHelpers.hasOp(s) => true
      case _                             => false
    }

    unparsed match {
      case Some(s) => abort(s"An operation was not expanded in $s (${s.pos})")
      case None    =>
    }

    // converts all Vista[A] to gen$vista.A or A depending on whether type was generated or not
    val expandVistaCheck: PartialFunction[Tree, Tree] = {
      case t: Type.Apply if t.syntax.matches(Constants.VistaTypeR) =>
        require(t.args.size == 1, () => "Must provide a single argument to Vista[A]")
        val clazzName = t.args.head.syntax

        if (db(clazzName).nonGenerated) Type.Name(clazzName)
        else Type.Select(Term.Name(Constants.GenName), Type.Name(clazzName))
    }

    val transformedObjects = transformed.map(_._2)
    templ
      .copy(stats = Option(generatedWrapper +: (transformedObjects ++ nstats)))
      .transform(expandVistaCheck) // transform all Vista[A] to either gen$vista.A or A
      .asInstanceOf[Template]
  }

  private def buildDatabase(tree: Tree): Unit = {
    tree.traverse {
      case c: Defn.Class => db.add(c)
      case t: Defn.Trait => db.add(t)
    }
  }

  private def buildConstructors(tree: Tree): Unit = {
    db.classes.map { inst =>
      inst.body
    }
  }
}
