package vista.operations

import vista.helpers.OpHelpers.isForbid
import vista.operations.internal.ForbidImpl
import vista.operations.parsers.{ForbidInput, parse}

import scala.meta._
import vista.semantics

/**
  * Public interface for Forbid method
  */
object Forbid extends Operation {
  def apply(defn: Defn.Val)(implicit db: semantics.Database.type): Term.Block =
    parse[Defn.Val, ForbidInput](defn) match {
      case None => throw new IllegalArgumentException("Couldn't parse defn") // FIXME: something more reasonable..
      case Some(de) => ForbidImpl(de)
    }

  def apply(defn: Defn.Def)(implicit db: semantics.Database.type): Tree =
    parse[Defn.Def, ForbidInput](defn) match {
      case None => throw new IllegalArgumentException("Couldn't parse defn") // FIXME: need something more reasonable
      case Some(t) => defn.copy(body = ForbidImpl(t))
    }

  override def modifier(implicit db: semantics.Database.type): PartialFunction[Tree, Term.Block] = {
    case defn: Defn.Val if isForbid(defn) => Forbid(defn)
  }
}




