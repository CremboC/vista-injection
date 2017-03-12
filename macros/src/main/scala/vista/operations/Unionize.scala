package vista.operations

import vista.operations.internal.UnionizeImpl
import vista.helpers.OpHelpers.isUnion
import vista.operations.parsers.UnionizeParser
import vista.semantics

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object Unionize extends Operation with UnionizeParser {

  def apply(definition: Defn.Val)(implicit db: semantics.Database.type): Term.Block = parseDefn(definition) match {
    case None => throw new IllegalArgumentException("Couldn't parse defn") // FIXME: something more reasonable..
    case Some(de) => UnionizeImpl(de)
  }

  override def modifier(implicit db: semantics.Database.type): PartialFunction[Tree, Term.Block] = {
    case defn: Defn.Val if isUnion(defn) => Unionize(defn)
  }
}






