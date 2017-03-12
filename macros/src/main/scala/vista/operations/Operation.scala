package vista.operations

import vista.semantics

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
trait Operation {
  def modifier(implicit db: semantics.Database.type): PartialFunction[Tree, Term.Block]
}
