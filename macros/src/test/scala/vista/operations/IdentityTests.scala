package vista.operations

import meta.xtensions.{XDefnIterable, XSet}
import org.scalatest._
import vista.operations.expanders.IntersectOp.Intersect
import vista.operations.expanders.UnionOp.Union
import vista.operations.parsers.OpVistas
import vista.{ResetsDatabase, semantics}

import scala.meta._
import scalaz.Scalaz.ToIdOps

/**
  * @author Paulius Imbrasas
  */
class IdentityTests extends FlatSpec with Matchers with ResetsDatabase {

  def asTermBlock(f: Tree): Term.Block =
    f.collect {
      case t: Defn.Trait => t.templ.stats.getOrElse(Seq.empty)
    }.flatten |> Term.Block.apply

  private val db = semantics.Database
  private val addInsts: Tree => Unit = _.traverse {
    case c: Defn.Class => db.add(c)
    case c: Defn.Trait => db.add(c)
  }

  // commutativity
  // A ∪ B = B ∪ A
  "Union" should "not be commutative" in {
    val classes =
      q"""
        class A {
          def a: Int = 1
          def b: Int = 2
        }

        class B {
          def a: Int = 1
          def c: Int = 3
        }
      """

    val source1 = q"""val ab: AB1 = ∪[A, B](a, b)"""
    val source2 = q"""val ab: AB2 = ∪[B, A](b, a)"""

    classes |> addInsts

    val f: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Union]

    f(source1) |> addInsts
    f(source2) |> addInsts

    val ab1methods = db("AB1").methods
    val ab2methods = db("AB2").methods

    // implies that union is commutative
    // which is only true when one looks at signatures only
    // hence, we first ensure it is commutative by signature
    val disjoint = ab1methods.signatures >+< ab2methods.signatures
    disjoint shouldBe empty

    // actual behaviour is different since the overrides are different
    // then, we ensure it is not commutative by normalised signature
    val normalizedDisjoint = ab1methods.normalized >+< ab2methods.normalized
    normalizedDisjoint should not be empty
  }

  // commutativity
  // A ∩ B = B ∩ A
  "Intersection" should "not be commutative" in {
    val classes =
      q"""
        class A {
          def a: Int = 1
          def b: Int = 2
        }

        class B {
          def a: Int = 1
          def c: Int = 3
        }
      """

    classes |> addInsts

    val f: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Intersect]

    q"val ab: AiB1 = ∩[A, B](a, b)" |> f |> addInsts
    q"val ab: AiB2 = ∩[B, A](b, a)" |> f |> addInsts

    val ab1methods = db("AiB1").methods
    val ab2methods = db("AiB2").methods

    // implies that intersection is commutative
    // which is only true when one looks at signatures only
    // which is true when only signatures are considered
    val disjoint = ab1methods.signatures >+< ab2methods.signatures
    disjoint shouldBe empty

    // we can compare by normalised defns
    val normalizedDisjoint = ab1methods.normalized >+< ab2methods.normalized
    normalizedDisjoint should not be empty
  }

  // associativity
  // (A ∪ B) ∪ C = A ∪ (B ∪ C)
  "Union" should "be associative" in {
    val classes =
      q"""
        class A {
          def a: Int = 1
          def b: Int = 2
        }

        class B {
          def a: Int = 1
          def c: Int = 3
        }

        class C {
          def f: Int = 0xf
        }
      """

    classes |> addInsts
    val f: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Union]

    // simulate left side
    // (A ∪ B) ∪ C
    q"val ab: AB = ∪[A, B](a, b)" |> f |> addInsts
    q"val abc: ABC = ∪[AB, C](ab, c)" |> f |> addInsts

    val abcmethods = db("ABC").methods

    // simulate right side
    // A ∪ (B ∪ C)
    q"val bc: BC = ∪[B, C](b, c)" |> f |> addInsts
    q"val abc: ABC2 = ∪[A, BC](a, bc)" |> f |> addInsts

    val abc2methods = db("ABC2").methods

    // which is true when only signatures are considered
    val disjoint = abcmethods.signatures >+< abc2methods.signatures
    disjoint shouldBe empty

    // we can compare by normalised defns
    val normalizedDisjoint = abcmethods.normalized >+< abc2methods.normalized
    normalizedDisjoint shouldBe empty
  }

  // associativity
  // (A ∩ B) ∩ C = A ∩ (B ∩ C)
  "Intersection" should "be associative" in {
    val classes =
      q"""
        class A {
          def a: Int = 1
          def b: Int = 2
        }

        class B {
          def a: Int = 1
          def c: Int = 3
        }

        class C {
          def f: Int = 0xf
        }
      """

    classes |> addInsts
    val f: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Intersect]

    // simulate left side
    // (A ∪ B) ∪ C
    q"val ab: AB = ∩[A, B](a, b)" |> f |> addInsts
    q"val abc: ABC = ∩[AB, C](ab, c)" |> f |> addInsts

    val abcmethods = db("ABC").methods

    // simulate right side
    // A ∪ (B ∪ C)
    q"val bc: BC = ∩[B, C](b, c)" |> f |> addInsts
    q"val abc: ABC2 = ∩[A, BC](a, bc)" |> f |> addInsts

    val abc2methods = db("ABC2").methods

    // which is true when only signatures are considered
    val disjoint = abcmethods.signatures >+< abc2methods.signatures
    disjoint shouldBe empty

    // we can compare by normalised defns
    val normalizedDisjoint = abcmethods.normalized >+< abc2methods.normalized
    normalizedDisjoint shouldBe empty
  }

  // distributivity
  // A ∪ (B ∩ C) = (A ∪ B) ∩ (A ∪ C)
  "Union and then intersection" should "be distributive" in {

  }

  // distributivity
  // A ∩ (B ∪ C) = (A ∩ B) ∪ (A ∩ C)

  // identity laws
  // A ∪ ∅ = A
  // A ∩ U = A

  // complement laws
  // A ∪ A' = U
  // A ∩ A' = ∅

  // idempotent
  // A ∪ A = A
  // A ∩ A = A

  // domination
  // A ∪ U = U
  // A ∩ ∅ = ∅

  // absorption
  // A ∪ (A ∩ B) = A
  // A ∩ (A ∪ B) = A

  // intersection as difference
  // A ∩ B = A \ (A \ B)

  // product non-communtativity and non-associativity
  // A ⨯ B ≠ B ⨯ A
  // (A ⨯ B) ⨯ C ≠ A ⨯ (B ⨯ C) -- unless one is empty
}
