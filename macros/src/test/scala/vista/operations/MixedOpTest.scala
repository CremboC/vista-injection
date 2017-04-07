package vista.operations

import vista.FlatSpecBase
import vista.operations.expanders.ForbidOp.Forbid
import vista.operations.expanders.ProductOp.Product
import vista.operations.expanders.UnionOp.Union
import vista.operations.parsers.{OpOverload, OpVistas}
import vista.util.Pipe._
import vista.util.meta.xtensions._

import scala.meta._

class MixedOpTest extends FlatSpecBase {
  "Forbid and then union" should "produce the correct result" in {
    val classes = q"class A { def a: Int = 1; def b: Int = 2 }"
    classes |> addInsts

    val union: (Term.Apply => Tree)  = parseAndExpand[Term.Apply, OpVistas, Union]
    val forbid: (Term.Apply => Tree) = parseAndExpand[Term.Apply, OpOverload, Forbid]

    val forbidA =
      q"""
        ∖[A, Ab](a, {
          def a: Int = ???
        })
      """
    forbidA |> forbid |> addInsts

    val forbidB =
      q"""
        ∖[A, Aa](a, {
          def b: Int = ???
        })
      """
    forbidB |> forbid |> addInsts

    val unionAaAb =
      q"""
        ∪[Aa, Ab, AauAb](aa, ab)
      """
    unionAaAb |> union |> addInsts

    db("AauAb").visibilities.signatures should contain only (
      q"def a: Int = {}".signature,
      q"def b: Int = {}".signature
    )

    db("AauAb").forbidden.signatures shouldBe empty
  }

  "Product then union" should "produce the correct result" in {
    val union: (Term.Apply => Tree)   = parseAndExpand[Term.Apply, OpVistas, Union]
    val product: (Term.Apply => Tree) = parseAndExpand[Term.Apply, OpVistas, Product]

    q"class A { def a(l: Boolean): Int = 1 }" |> addInsts
    q"class B { def b: Int = 2 }" |> addInsts
    q"class C { def c: Int = 3 }" |> addInsts

    q"x[A, B, AxB](a, b)" |> product |> addInsts
    q"∪[AxB, C, ABuC](axb, c)" |> union |> addInsts

    db("AxB").visibilities.signatures should contain only q"def ab(p1: Boolean)() = {}".signature
    db("ABuC").visibilities.signatures should contain only (
      q"def ab(p1: Boolean)() = {}".signature,
      q"def c = {}".signature
    )

    q"class D { def d: Int = 4 }" |> addInsts
    q"x[D, ABuC, DxABuC](d, abuc)" |> product |> addInsts

    db("DxABuC").visibilities.signatures should contain only (
      q"def dab()(p1: Boolean)() = {}".signature,
      q"def dc()() = {}".signature
    )
  }

}
