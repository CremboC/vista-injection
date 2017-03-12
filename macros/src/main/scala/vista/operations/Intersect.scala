package vista.operations

import vista.semantics

import scala.meta._

/**
  * Created by Crembo on 2017-03-08.
  */
object Intersect extends Operation {
  def apply(defn: Defn.Val): Tree = ???

  override def modifier(implicit db: semantics.Database.type): PartialFunction[Tree, Term.Block] = ???
}
