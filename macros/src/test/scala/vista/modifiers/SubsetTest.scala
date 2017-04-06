package vista.modifiers

import vista.FlatSpecBase
import vista.util.Pipe._

import scala.meta._

class SubsetTest extends FlatSpecBase {
  behavior of "Subset"

  it should "correctly detect the subset in a simple case (postfix)" in {
    q"class A { def a: Int = 1 }" |> addInsts
    val subset = Subset(q"{ def a: Int = ??? } ⊆[A] a")

    subset should equal(Lit.Boolean(true))
  }

  it should "correctly detect the subset in a simple case (postfix, negative)" in {
    q"class A { def a: Int = 1 }" |> addInsts
    val subset = Subset(q"{ def f: Int = ??? } ⊆[A] a")

    subset should equal(Lit.Boolean(false))
  }

  it should "correctly detect the subset in a simple case" in {
    q"class A { def a: Int = 1 }" |> addInsts
    val subset = Subset(q"{ def a: Int = ??? }.⊆[A](a)")

    subset should equal(Lit.Boolean(true))
  }

  it should "correctly detect the subset in a simple case (negative)" in {
    q"class A { def a: Int = 1 }" |> addInsts
    val subset = Subset(q"{ def f: Int = ??? }.⊆[A](a)")

    subset should equal(Lit.Boolean(false))
  }

  it should "correctly detect the subset in a simple case (partial postfix)" in {
    q"class A { def a: Int = 1 }" |> addInsts
    val subset = Subset(q"{ def a: Int = ??? } ⊆[A](a)")

    subset should equal(Lit.Boolean(true))
  }

  it should "correctly detect the subset in a simple case (partial postfix, negative)" in {
    q"class A { def a: Int = 1 }" |> addInsts
    val subset = Subset(q"{ def f: Int = ??? } ⊆[A](a)")

    subset should equal(Lit.Boolean(false))
  }

  it should "correctly detect the subset in a complex case (hierarchy)" in {
    q"class A { def a: Int = 1 }" |> addInsts
    q"class B extends A { def f: Int = 1 }" |> addInsts
    val subset = Subset(q"{ def a: Int = ??? } ⊆[B] b")

    subset should equal(Lit.Boolean(true))
  }
}
