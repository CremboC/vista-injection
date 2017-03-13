import scala.meta._
import scala.meta.contrib._
import scala.collection.immutable.Seq

/**
  * @author Paulius Imbrasas
  */
package object meta {

  /**
    * Defn.Def extensions.
    * @param defn
    */
  implicit class XDefn(defn: Defn.Def) {
    def signature: Defn.Def = defn.copy(body = Term.Block(Seq.empty), decltpe = None)
  }

  /**
    * Extensions for sets of trees.
    *
    * @param self set of any element which can be considered a tree
    * @tparam A anything that is a tree
    */
  implicit class XMetaIterable[A <: Tree](self: Set[A]) {

    def structurally: Set[Structurally[A]] = self.map(new Structurally(_))
    def syntactically: Set[Syntactically[A]] = self.map(new Syntactically(_))

    def mintersect(other: Set[A]): Set[A] = {
      val selfStr = self.map(new Structurally(_))
      val otherStr = other.map(new Structurally(_))

      selfStr.intersect(otherStr).map(_.tree)
    }

    def mdiff(other: Set[A]): Set[A] = {
      val selfStr = self.map(new Structurally(_))
      val otherStr = other.map(new Structurally(_))

      (selfStr.diff(otherStr) ++ otherStr.diff(selfStr)).map(_.tree)
    }
  }

  implicit class XDefnIterable[A <: Defn.Def](self: Iterable[A]) {
    def signatures: Iterable[Defn.Def] = self.map(_.signature)
  }
}
