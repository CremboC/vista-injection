package vista.util

import scala.meta.Tree

object Extractable {
  trait Extractable {
    def unapply(arg: Tree): Boolean
  }

  trait Extractable1[A <: Tree] {
    def unapply[B <: Tree](arg: B): Option[A]
  }

  implicit class ExtractableOp(val ex: Extractable) extends AnyVal {
    def asPartial: PartialFunction[Tree, Boolean] = {
      case ex() => true
      case _    => false
    }
  }

  implicit class Extractable1Op(val ex: Extractable1[_]) extends AnyVal {
    def asPartial[A <: Tree]: PartialFunction[A, Boolean] = {
      case ex(_) => true
      case _     => false
    }
  }
}