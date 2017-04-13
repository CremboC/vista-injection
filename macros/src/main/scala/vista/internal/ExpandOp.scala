package vista.internal

import vista.Constants.forbiddenMethodBody
import vista.helpers.OpHelpers
import vista.operations.expanders._
import vista.operations.parsers.{OpInput, OpOverload, OpVistas, Parser}
import vista.semantics.Database.ClassName
import vista.util.Pipe._

import scala.collection.immutable.{Queue, Seq}
import scala.meta._
import scala.meta.contrib._

private[internal] object ExpandOp {

  private val db = vista.semantics.Database

  private type Operation = (OpInput, (OpInput => Defn.Trait))
  private case class EvalUnit(r: Operation, seen: Boolean)

  def apply(defn: Tree): Seq[Defn.Trait] = {
    // collect all terms that have a vista operator
    val terms = defn.collect(OpHelpers.HasOp.asResultingPartial).flatten

    // parse all terms and get their respective expander
    val results = terms.map(termToOp).asInstanceOf[List[Operation]]

    evaluate(results.map(EvalUnit(_, seen = false)).to)
  }

  private def termToOp(t: Term.Apply) = t match {
    case term @ OpHelpers.OpVistas() =>
      val expander = term match {
        case OpHelpers.Forbid(_)    => Expander[OpVistas, ForbidOp.Forbid]
        case OpHelpers.Union(_)     => Expander[OpVistas, UnionOp.Union]
        case OpHelpers.Intersect(_) => Expander[OpVistas, IntersectOp.Intersect]
        case OpHelpers.Product(_)   => Expander[OpVistas, ProductOp.Product]
      }

      (Parser[Term.Apply, OpVistas].parse(term), expander.expand _)

    case term @ OpHelpers.OpOverload() =>
      val expander = term match {
        case OpHelpers.Forbid(_) => Expander[OpOverload, ForbidOp.Forbid]
      }
      (Parser[Term.Apply, OpOverload].parse(term), expander.expand _)
  }

  // function to easily add generated types into semdb
  private def recordClass(g: Defn.Trait) = {
    db.add(g, generated = true)
    g
  }

  // function to add compile-time error annotation to forbidden methods
  private def forbidMethods(g: Defn.Trait) =
    g.transform {
        case d: Defn.Def if d.body isEqual forbiddenMethodBody =>
          d.copy(mods = restrictAnnotation(d.name.value, g.name.value) +: d.mods)
      }
      .asInstanceOf[Defn.Trait]

  private def evaluate(queue: Queue[EvalUnit]): List[Defn.Trait] = {
    if (queue.isEmpty) List.empty
    else {
      val (head, tail)      = queue.dequeue
      val (input, expander) = head.r
      val canExpand = input match {
        case OpVistas(lclass, rclass, _, _, _) =>
          db.exists(lclass) && db.exists(rclass)
        case OpOverload(lclass, _, _, _) =>
          db.exists(lclass)
      }

      if (head.seen && !canExpand) {
        val error: ClassName => String =
          c => if (!db.exists(c)) s"$c was never declared or generated" else ""

        val errors = input match {
          case OpVistas(lclass, rclass, _, _, _) => Seq(error(lclass), error(rclass))
          case OpOverload(lclass, _, _, _)       => Seq(error(lclass))
        }

        abort(s"Attempted to evaluate $input. Errors are: ${errors.mkString("")}")
      }

      if (canExpand) {
        val expanded = input |> (expander andThen recordClass andThen forbidMethods)
        expanded :: evaluate(tail)
      } else evaluate(tail.enqueue(head.copy(seen = true)))
    }
  }
}
