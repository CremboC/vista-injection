package vista.operations

import vista.FlatSpecBase
import vista.operations.expanders.ForbidOp.Forbid
import vista.operations.expanders.UnionOp.Union
import vista.operations.parsers.{OpOverload, OpVistas}

import scala.meta._
import scalaz.Scalaz.ToIdOps

/**
  * Created by Crembo on 2017-03-22.
  */
class MixedOpTest extends FlatSpecBase {
  "Forbid and then union" should "produce the correct result" in {
    val classes = q"class A { def a: Int = 1; def b: Int = 2 }"
    classes |> addInsts

    val union: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Union]
    val forbid: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpOverload, Forbid]

    // forbid a
    val forbidA =
      q"""
        val ab: Ab = ∖[A](a, {
          def a: Int = ???
        })
      """
    forbidA |> forbid |> addInsts

    val forbidB =
      q"""
        val aa: Aa = ∖[A](a, {
          def b: Int = ???
        })
      """
    forbidB |> forbid |> addInsts

    val unionAaAb =
      q"""
        val ab: AauAb = ∪[Aa, Ab](aa, ab)
      """
    unionAaAb |> union |> addInsts



    db("AauAb").visibilities
  }

}
