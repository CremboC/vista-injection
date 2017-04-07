package vista

import vista.Constants.forbiddenMethodBody
import vista.helpers.OpHelpers
import vista.modifiers._
import vista.operations.expanders._
import vista.operations.parsers.{OpInput, OpOverload, OpVistas, Parser}
import vista.semantics.Database.ClassName
import vista.semantics.Inst
import vista.util.Pipe._

import scala.annotation.StaticAnnotation
import scala.collection.immutable.{Queue, Seq}
import scala.meta.Term.Block
import scala.meta._
import scala.meta.contrib._

object Vista {
  private val db = semantics.Database

  private def expandOps(defn: Tree): Seq[Defn.Trait] = {
    val terms = defn.collect(OpHelpers.HasOp.asResultingPartial).flatten

    type Result = (OpInput, (OpInput => Defn.Trait))
    val generated = terms.map {
      case term@OpHelpers.OpVistas() =>
        val expander = term match {
          case OpHelpers.Forbid(_) => Expander[OpVistas, ForbidOp.Forbid]
          case OpHelpers.Union(_) => Expander[OpVistas, UnionOp.Union]
          case OpHelpers.Intersect(_) => Expander[OpVistas, IntersectOp.Intersect]
          case OpHelpers.Product(_) => Expander[OpVistas, ProductOp.Product]
        }

        (Parser[Term.Apply, OpVistas].parse(term), expander.expand _)

      case term@OpHelpers.OpOverload() =>
        val expander = term match {
          case OpHelpers.Forbid(_) => Expander[OpOverload, ForbidOp.Forbid]
        }
        (Parser[Term.Apply, OpOverload].parse(term), expander.expand _)
    }.asInstanceOf[List[Result]]

    val addGenerated = (g: Defn.Trait) => { db.add(g, generated = true); g }
    val forbidMethods = (g: Defn.Trait) =>
      g.transform {
        case d: Defn.Def if d.body isEqual forbiddenMethodBody =>
          d.copy(mods = restrictAnnotation(d.name.value, g.name.value) +: d.mods)
      }.asInstanceOf[Defn.Trait]

    case class EvalUnit(r: Result, seen: Boolean)

    def evaluate(queue: Queue[EvalUnit]): List[Defn.Trait] = {
      if (queue.isEmpty) List.empty
      else {
        val (head, tail) = queue.dequeue
        val (input, expander) = head.r
        val canExpand = input match {
          case OpVistas(lclass, rclass, _, _, newtype) =>
            db.exists(lclass) && db.exists(rclass)
          case OpOverload(lclass, _, newtype, _) =>
            db.exists(lclass)
        }

        if (head.seen && !canExpand) {
          val error: ClassName => String =
            c => if (!db.exists(c)) s"$c was never declared or generated" else ""

          val errors = input match {
            case OpVistas(lclass, rclass, _, _, _) => Seq(error(lclass), error(rclass))
            case OpOverload(lclass, _, _, _) => Seq(error(lclass))
          }

          abort(s"Attempted to evaluate $input. Errors are: ${errors.mkString("")}")
        }

        if (canExpand) {
          val expanded = input |> (expander andThen addGenerated andThen forbidMethods)
          expanded :: evaluate(tail)
        } else evaluate(tail.enqueue(head.copy(seen = true)))
      }
    }

    evaluate(generated.map(EvalUnit(_, seen = false)).to)
  }

  def expand(obj: Defn.Object): Defn.Object = {
    def classIsRecorded(term: Term.New): Boolean =
      if (term.templ.parents.isEmpty) false
      else {
        val className = term.templ.parents.head.syntax.takeWhile(_ != '(').trim
        db.exists(className) && db.get(className).isInstanceOf[Inst.Class]
      }

    val template"{ ..$_ } with ..$_ { $_ => ..$stats }" = obj.templ

    // build up SemDB
    obj.traverse {
      case c: Defn.Class => db.add(c)
      case t: Defn.Trait => db.add(t)
    }

    val traits = Vista.expandOps(obj)
    val generatedWrapper = q"object ${Term.Name(Constants.GenName)} { ..$traits }"

    val Block(nstats) = Block(stats)
      .transform { // first convert all classes into traits
        case classdefn: Defn.Class if !OpHelpers.hasCaseMod(classdefn) => Tratify(classdefn)
      }
      .transform {
        case term: Term.New if classIsRecorded(term) =>
          Tratify(term)

        case OpHelpers.Subset(t) =>
          Subset(t)

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
        case s@q"$t.invoke[$typ](..$funs)(..$args)" if db.exists(typ.syntax) =>
          // FIXME: this is really bad, I am truly sorry
          val reduce = funs.map(_.syntax).reduce(_ + _)
          val argss = args.map(_.syntax).reduce(_ + _)
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

        if (db(clazzName).notGenerated) Type.Name(clazzName)
        else Type.Select(Term.Name(Constants.GenName), Type.Name(clazzName))
    }

//    val reflectiveImport = q"import scala.language.reflectiveCalls"
    val ntemplate = obj
      .templ
      .copy(stats = Option(generatedWrapper +: nstats))
      .transform(expandVistaCheck) // transform all Vista[A] to either gen$vista.A or A
      .asInstanceOf[Template]

    obj.copy(templ = ntemplate)
  }
}

class enable extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case obj: Defn.Object => Vista.expand(obj)
      case _ => abort("This annotation must be placed on an object")
    }
  }
}
