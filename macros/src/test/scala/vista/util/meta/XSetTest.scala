package vista.util.meta

import org.scalatest._
import vista.util.meta.xtensions._

import scala.meta._
import scala.meta.contrib._

/**
  * @author Paulius Imbrasas
  */
class XSetTest extends FlatSpec with Matchers {
  "A vista.util.meta iterable" should "find the intersection correctly" in {
    val s1 = Set(q"def one(): Int = 1", q"def two(): Int = 2")
    val s2 = Set(q"def one(): Int = 1", q"def three(): Int = 3")

    s1.mintersect(s2) should equal(
      Set(
        s1.head
      ))
  }

  "A vista.util.meta iterable" should "find the disjoint union correctly" in {
    val s1 =
      q"""
          def a: String = "a"
          def b: String = "b"
          def c: String = "c"
        """.collect { case d: Defn.Def => d }.toSet

    val s2 =
      q"""
          def b: String = "b"
          def d: String = "d"
        """.collect { case d: Defn.Def => d }.toSet

    val expected =
      q"""
          def a: String = "a"
          def c: String = "c"
          def d: String = "d"
        """.collect { case d: Defn.Def => d }.toSet

    val result = s1.symmDiff(s2).structurally
    expected.structurally.forall(result.contains) should be(true)
  }
}
