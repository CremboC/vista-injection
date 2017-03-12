package vista.operations

import vista.helpers.OpHelpers.isForbid
import vista.operations.internal.ForbidImpl

import scala.meta._
import vista.semantics

import scala.collection.immutable.Seq

/**
  * Public interface for Forbid method
  */
object Forbid extends Operation {
  import vista.operations.internal.ForbidImpl._

  def apply(defn: Defn.Val)(implicit db: semantics.Database.type): Term.Block = parseDefn(defn) match {
    case None => throw new IllegalArgumentException("Couldn't parse defn") // FIXME: something more reasonable..
    case Some(de) => ForbidImpl(de)
  }

  def apply(defn: Defn.Def)(implicit db: semantics.Database.type): Tree = parseDefn(defn) match {
    case None => throw new IllegalArgumentException("Couldn't parse defn") // FIXME: need something more reasonable
    case Some(t) => defn.copy(body = ForbidImpl(t))
  }

  override def modifier(implicit db: semantics.Database.type): PartialFunction[Tree, Term.Block] = {
    case defn: Defn.Val if isForbid(defn) => Forbid(defn)
  }
}


private[operations] case class ForbidInput(
                        nclass: String,
                        oclass: String,
                        methods: Seq[Defn.Def],
                        varname: Option[String] = None,
                        lsource: Option[String] = None,
                        rsource: Option[String] = None
                      )

