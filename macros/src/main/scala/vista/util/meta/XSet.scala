package vista.util.meta

import scala.meta.Tree
import scala.meta.contrib.{Structurally, Syntactically}

trait XSet {

  /**
    * Extensions for sets of trees.
    *
    * @param self set of any element which can be considered a tree
    * @tparam A anything that is a tree
    */
  implicit class XSet[A <: Tree](self: Set[A]) {

    @inline
    def structurally: Set[Structurally[A]] = self.map(new Structurally(_))

    @inline
    def syntactically: Set[Syntactically[A]] = self.map(new Syntactically(_))

    def mintersect(other: Set[A]): Set[A] = {
      val selfStr  = self.structurally
      val otherStr = other.structurally

      selfStr.intersect(otherStr).map(_.tree)
    }

    /**
      * A <-> B = (A \ B) u (B \ A)
      */
    def symmDiff(other: Set[A]): Set[A] = {
      val selfStr  = self.structurally
      val otherStr = other.structurally

      (selfStr.diff(otherStr) ++ otherStr.diff(selfStr)).map(_.tree)
    }

    def <->(other: Set[A]): Set[A] = symmDiff(other)

    /**
      * Difference
      */
    def mdiff(other: Set[A]): Set[A] = {
      self.structurally.diff(other.structurally).map(_.tree)
    }

    def \(other: Set[A]): Set[A] = mdiff(other)

    def cross(other: Set[A]): Set[(A, A)] = {
      for { s <- self; o <- other } yield (s, o)
    }

    def ><(other: Set[A]): Set[(A, A)] = cross(other)
  }
}
