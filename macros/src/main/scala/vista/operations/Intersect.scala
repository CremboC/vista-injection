package vista.operations

import vista.helpers.OpHelpers.isIntersect
import vista.operations.internal.IntersectImpl
import vista.operations.parsers.IntersectParser
import vista.semantics

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object Intersect extends Operation with IntersectParser {

  def apply(definition: Defn.Val)(implicit db: semantics.Database.type): Term.Block = parseDefn(definition) match {
    case None => throw new IllegalArgumentException("Couldn't parse defn") // FIXME: something more reasonable..
    case Some(de) => IntersectImpl(de)
  }

  override def modifier(implicit db: semantics.Database.type): PartialFunction[Tree, Term.Block] = {
    case defn: Defn.Val if isIntersect(defn) => Unionize(defn)
  }
}

