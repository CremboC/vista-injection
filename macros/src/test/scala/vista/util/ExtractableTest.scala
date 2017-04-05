package vista.util

import vista.FlatSpecBase

import scala.meta._

class ExtractableTest extends FlatSpecBase {
  behavior of "Extractable"

  it should "extract correctly when parsing a source" in {
    val termApplyEx = new Extractable.Extractable {
      override def unapply(arg: Tree): Boolean = arg match {
        case _: Term.Apply => true
        case _             => false
      }
    }

    val source = q"""
        object A {
          val a = Seq(1, 2, 3)
          a.map(_ * 2)
        }
    """

    val collect = source.collect(termApplyEx.asPartial).filter(_ == true)
    collect should have size 2
  }

  it should "extract with the type correctly when parsing a source" in {
    val termApplyEx = new Extractable.Extractable1[Term.Apply] {
      override def unapply[B <: Tree](arg: B): Option[Term.Apply] = arg match {
        case t: Term.Apply => Option(t)
        case _             => None
      }
    }

    val source = q"""
        object A {
          val a = Seq(1, 2, 3)
          a.map(_ * 2)
        }
    """

    val collect = source.collect(termApplyEx.asResultingPartial).flatten
    collect should have size 2

    collect should contain only (q"Seq(1, 2, 3)", q"a.map(_ * 2)")
  }

  it should "detect a desired instance in a single statement" in {
    val source = q"a.map(_ * 2)"
    val termApplyEx = new Extractable.Extractable {
      override def unapply(arg: Tree): Boolean = arg match {
        case _: Term.Apply => true
        case _             => false
      }
    }

    termApplyEx.unapply(source) should be(true)
  }

  it should "fail to detect the desired instance in a wrong single statement" in {
    val source = q"a.map(_ * 2)"
    val defnDefEx = new Extractable.Extractable {
      override def unapply(arg: Tree): Boolean = arg match {
        case _: Defn.Def => true
        case _           => false
      }
    }

    defnDefEx.unapply(source) should be(false)
  }

  it should "extract desired instance in a single statement" in {
    val source = q"def a: Int = 5"
    val defnDefEx = new Extractable.Extractable1[Defn.Def] {
      override def unapply[B <: Tree](arg: B): Option[Defn.Def] = arg match {
        case t: Defn.Def => Option(t)
        case _           => None
      }
    }

    val defnDefEx(result) = source
    result should equal(q"def a: Int = 5")
  }

  it should "fail to extract desired instance in a wrong single statement" in {
    val source = q"val a = 5 * 5"
    val defnDefEx = new Extractable.Extractable1[Defn.Def] {
      override def unapply[B <: Tree](arg: B): Option[Defn.Def] = arg match {
        case t: Defn.Def => Option(t)
        case _           => None
      }
    }

    assertThrows[MatchError] {
      val defnDefEx(result) = source
    }
  }

}
