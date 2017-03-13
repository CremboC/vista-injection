package vista.operations

import vista.operations.internal.ProductImpl
import vista.semantics
import vista.operations.parsers.{ProductInput, parse}

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object Product extends Operation {
  def apply(defn: Defn.Val)(implicit db: semantics.Database.type): Term.Block =
    parse[Defn.Val, ProductInput](defn) match {
      case None => throw new IllegalArgumentException("Couldn't parse defn") // FIXME: something more reasonable..
      case Some(de) => ProductImpl(de)
    }

  override def modifier(implicit db: semantics.Database.type): PartialFunction[Tree, Term.Block] = ???
}