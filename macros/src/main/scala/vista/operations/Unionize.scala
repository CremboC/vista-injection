package vista.operations

import vista.operations.internal.UnionizeImpl

import vista.helpers.OpHelpers.isUnion
import vista.semantics

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object Unionize extends Operation {
  import vista.operations.internal.UnionizeImpl._

  def apply(definition: Defn.Val)(implicit db: semantics.Database.type): Term.Block = parseDefn(definition) match {
    case None => throw new IllegalArgumentException("Couldn't parse defn") // FIXME: something more reasonable..
    case Some(de) => UnionizeImpl(de)
  }

  override def modifier(implicit db: semantics.Database.type): PartialFunction[Tree, Term.Block] = {
    case defn: Defn.Val if isUnion(defn) => Unionize(defn)
  }
}

private[operations] case class UnionizeInput(
                                              lclass: String,
                                              rclass: String,
                                              lvar: String,
                                              rvar: String,
                                              newtype: String,
                                              newvar: Option[String] = None
                                            )

