package vista.operations

import vista.operations.parsers.{OpInput, OpOverload, OpVistas}

import scala.meta.Term

/**
  * @author Paulius Imbrasas
  */
package object expanders {
  sealed trait Op[A]

  trait Expander[A <: OpInput, B <: Op[_]] {
    def expand(input: A): Term.Block
  }

  implicit val forbidExpander: Expander[OpOverload, ForbidOp.Forbid] = ForbidOp.expander
  implicit val intersectExpander: Expander[OpVistas, IntersectOp.Intersect] = IntersectOp.expander
  implicit val unionExpander: Expander[OpVistas, UnionOp.Union] = UnionOp.expander
//  implicit val forbidExpander: Expander[OpOverload] = ForbidOp.expander

  def expand[A <: OpInput, B <: Op[_]](input: A)(implicit expander: Expander[A, B]): Term.Block = expander.expand(input)
}
