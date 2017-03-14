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
    def signature: Defn.Def = {
      // ensures that def g() === def g
      val paramss = if (defn.paramss.isEmpty) Seq(Seq.empty) else defn.paramss
      defn.copy(body = Term.Block(Seq.empty), decltpe = None, paramss = paramss)
    }
  }

  /**
    * Extensions for sets of trees.
    *
    * @param self set of any element which can be considered a tree
    * @tparam A anything that is a tree
    */
  implicit class XMetaIterable[A <: Tree](self: Set[A]) {

    @inline
    def structurally: Set[Structurally[A]] = self.map(new Structurally(_))

    @inline
    def syntactically: Set[Syntactically[A]] = self.map(new Syntactically(_))

    def mintersect(other: Set[A]): Set[A] = {
      val selfStr = self.structurally
      val otherStr = other.structurally

      selfStr.intersect(otherStr).map(_.tree)
    }

    def disjointUnion(other: Set[A]): Set[A] = {
      val selfStr = self.structurally
      val otherStr = other.structurally

      (selfStr.diff(otherStr) ++ otherStr.diff(selfStr)).map(_.tree)
    }

    /**
      * Alias for disjoint union
      */
    def >+<(other: Set[A]): Set[A] = disjointUnion(other)

    /**
      * Difference
      */
    def mdiff(other: Set[A]): Set[A] = {
      self.structurally.diff(other.structurally).map(_.tree)
    }

    def \(other: Set[A]): Set[A] = mdiff(other)


    def cross(other: Set[A]): Set[(A, A)] = {
      val selfStr = self.structurally
      val otherStr = other.structurally

      val pairs = for { s <- selfStr; o <- otherStr } yield (s, o)
      pairs.map {
        case (x, y) => (x.tree, y.tree)
      }
    }

    def ><(other: Set[A]): Set[(A, A)] = cross(other)
  }

  implicit class XDefnIterable[A <: Defn.Def](self: Iterable[A]) {
    @inline
    def signatures: Set[Defn.Def] = self.map(_.signature).toSet
  }
}
