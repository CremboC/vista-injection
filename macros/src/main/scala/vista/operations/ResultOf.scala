package vista.operations

import vista.operations.expanders.{Op, ProductOp}
import vista.semantics.Inst

import scala.meta._
import scala.meta.contrib._

trait ResultOf[A <: Op[_]] {
  def check(inst: Inst): Boolean
}

object ResultOf {
  implicit object ProductResultOf extends ResultOf[ProductOp.Product] {
    override def check(inst: Inst): Boolean = inst.body.mods.exists(_ isEqual mod"@vista.product")
  }

  def apply[A <: Op[_]: ResultOf]: ResultOf[A] = implicitly
}
