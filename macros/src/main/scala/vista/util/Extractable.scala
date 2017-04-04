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

  implicit class Extractable1Op[B <: Tree](val ex: Extractable1[B]) extends AnyVal {
    def asPartial[A <: Tree]: PartialFunction[A, Boolean] = {
      case ex(_) => true
      case _     => false
    }

    def asResultingPartial[A <: Tree]: PartialFunction[A, Option[B]] = {
      case ex(f) => Option(f)
      case _     => None
    }
  }
}
