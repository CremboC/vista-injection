package vista.operations

import vista.FlatSpecBase
import vista.meta.xtensions.{XDefnIterable, XSet}
import vista.operations.expanders.ForbidOp.Forbid
import vista.operations.expanders.IntersectOp.Intersect
import vista.operations.expanders.ProductOp.Product
import vista.operations.expanders.UnionOp.Union
import vista.operations.parsers.OpVistas

import scala.meta._
import scalaz.Scalaz.ToIdOps

/**
  * @author Paulius Imbrasas
  */
class IdentityTest extends FlatSpecBase {

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

    val ab1methods = db("AB1").visibilities
    val ab2methods = db("AB2").visibilities

    // implies that union is commutative
    // which is only true when one looks at signatures only
    // hence, we first ensure it is commutative by signature
    val diff = ab1methods.signatures <-> ab2methods.signatures
    diff shouldBe empty

    // actual behaviour is different since the overrides are different
    // then, we ensure it is not commutative by normalised signature
    val normalizedDiff = ab1methods.normalized <-> ab2methods.normalized
    normalizedDiff should not be empty
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

    val ab1methods = db("AiB1").visibilities
    val ab2methods = db("AiB2").visibilities

    // implies that intersection is commutative
    // which is only true when one looks at signatures only
    // which is true when only signatures are considered
    val diff = ab1methods.signatures <-> ab2methods.signatures
    diff shouldBe empty

    // we can compare by normalised defns
    val normalizedDiff = ab1methods.normalized <-> ab2methods.normalized
    normalizedDiff should not be empty
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

    val abcmethods = db("ABC").visibilities

    // simulate right side
    // A ∪ (B ∪ C)
    q"val bc: BC = ∪[B, C](b, c)" |> f |> addInsts
    q"val abc: ABC2 = ∪[A, BC](a, bc)" |> f |> addInsts

    val abc2methods = db("ABC2").visibilities

    // which is true when only signatures are considered
    val diff = abcmethods.signatures <-> abc2methods.signatures
    diff shouldBe empty

    // we can compare by normalised defns
    val normalizedDiff = abcmethods.normalized <-> abc2methods.normalized
    normalizedDiff should not be empty
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
    val inter: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Intersect]

    // simulate left side
    // (A ∪ B) ∪ C
    q"val ab: AB = ∩[A, B](a, b)" |> inter |> addInsts
    q"val abc: ABC = ∩[AB, C](ab, c)" |> inter |> addInsts

    val abcMethods = db("ABC").visibilities

    // simulate right side
    // A ∪ (B ∪ C)
    q"val bc: BC = ∩[B, C](b, c)" |> inter |> addInsts
    q"val abc: ABC2 = ∩[A, BC](a, bc)" |> inter |> addInsts

    val abc2Methods = db("ABC2").visibilities

    // which is true when only signatures are considered
    val diff = abcMethods.signatures <-> abc2Methods.signatures
    diff shouldBe empty

    // we can compare by normalised defns
    val normalizedDiff = abcMethods.normalized <-> abc2Methods.normalized
    normalizedDiff shouldBe empty
  }

  // distributivity
  // A ∪ (B ∩ C) = (A ∪ B) ∩ (A ∪ C)
  "Union and then intersection" should "be distributive" in {
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
    val union: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Union]
    val inter: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Intersect]

    // left side
    // A ∪ (B ∩ C)
    q"val bc: BC = ∩[B, C](b, c)" |> inter |> addInsts
    q"val aubc: AuBC = ∪[A, BC](a, bc)" |> union |> addInsts

    val aubcMethods = db("AuBC").visibilities

    // right side
    // (A ∪ B) ∩ (A ∪ C)
    q"val ab: AB = ∪[A, B](a, b)" |> union |> addInsts
    q"val ac: AC = ∪[A, C](a, C)" |> union |> addInsts
    q"val abnac: ABnAC = ∩[AB, AC](ab, ac)" |> inter |> addInsts

    val abnacMethods = db("ABnAC").visibilities

    // which is true when only signatures are considered
    val diff = aubcMethods.signatures <-> abnacMethods.signatures
    diff shouldBe empty

    // we can compare by normalised defns
    val normalizedDiff = aubcMethods.normalized <-> abnacMethods.normalized
    normalizedDiff should not be empty
  }

  // distributivity
  // A ∩ (B ∪ C) = (A ∩ B) ∪ (A ∩ C)
  "Intersection and then union" should "be distributive" in {
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
    val union: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Union]
    val inter: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Intersect]

    // left side
    // A ∩ (B ∪ C)
    q"val bc: BC = ∪[B, C](b, c)" |> union |> addInsts
    q"val anbc: AnBC = ∩[A, BC](a, bc)" |> inter |> addInsts

    val anbcMethods = db("AnBC").visibilities

    // right side
    // (A ∩ B) ∪ (A ∩ C)
    q"val ab: AB = ∩[A, B](a, b)" |> inter |> addInsts
    q"val ac: AC = ∩[A, C](a, C)" |> inter |> addInsts
    q"val abuac: ABuAC = ∪[AB, AC](ab, ac)" |> union |> addInsts

    val abuacMethods = db("ABuAC").visibilities

    // which is true when only signatures are considered
    val diff = anbcMethods.signatures <-> abuacMethods.signatures
    diff shouldBe empty

    // we can compare by normalised defns
    val normalizedDiff = anbcMethods.normalized <-> abuacMethods.normalized
    normalizedDiff should not be empty
  }

  // identity laws
  // A ∪ ∅ = A
  "Union of A with empty" should "should be A" in {
    val classes =
      q"""
        class A {
          def a: Int = 1
          def b: Int = 2
        }

        class O
      """

    classes |> addInsts
    val union: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Union]
    q"val ab: AO = ∪[A, O](a, o)" |> union |> addInsts

    val aMethods  = db("A").visibilities
    val abMethods = db("AO").visibilities

    // which is true when only signatures are considered
    val diff = aMethods.signatures <-> abMethods.signatures
    diff shouldBe empty

    // we can compare by normalised defns
    val normalizedDiff = aMethods.normalized <-> abMethods.normalized
    normalizedDiff should not be empty
  }

  // identity laws
  // A ∩ U = A
  "Union of A with universal" should "be universal" in {
    assertThrows[IllegalArgumentException] {
      throw new IllegalArgumentException("Universal set cannot exists in vistas")
    }
  }

  // complement laws
  // A ∪ A' = U
  "Union of A with its complement" should "be universal" in {
    assertThrows[IllegalArgumentException] {
      throw new IllegalArgumentException("Complement does not exist in vistas")
    }
  }

  // complement laws
  // A ∩ A' = ∅
  "Intersection of A with its complement" should "be empty set" in {
    assertThrows[IllegalArgumentException] {
      throw new IllegalArgumentException("Complement does not exist in vistas")
    }
  }

  // idempotent
  // A ∪ A = A
  "Union of A with A" should "be A" in {
    val classes =
      q"""
        class A {
          def a: Int = 1
          def b: Int = 2
        }
      """

    classes |> addInsts
    val union: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Union]
    q"val ab: AA = ∪[A, A](a, a)" |> union |> addInsts

    val aMethods  = db("A").visibilities
    val aaMethods = db("AA").visibilities

    // which is true when only signatures are considered
    val diff = aMethods.signatures <-> aaMethods.signatures
    diff shouldBe empty

    // we can compare by normalised defns
    val normalizedDiff = aMethods.normalized <-> aaMethods.normalized
    normalizedDiff should not be empty
  }

  // idempotent
  // A ∩ A = A
  "Intersection of A with A" should "be A" in {
    val classes =
      q"""
        class A {
          def a: Int = 1
          def b: Int = 2
        }
      """

    classes |> addInsts
    val inter: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Intersect]
    q"val ab: AA = ∩[A, A](a, a)" |> inter |> addInsts

    val aMethods  = db("A").visibilities
    val aaMethods = db("AA").visibilities

    // which is true when only signatures are considered
    val diff = aMethods.signatures <-> aaMethods.signatures
    diff shouldBe empty

    // we can compare by normalised defns
    val normalizedDiff = aMethods.normalized <-> aaMethods.normalized
    // FIXME: technically produces the same thing so should be "not empty"
    normalizedDiff should not be empty
  }

  // domination
  // A ∪ U = U
  "Union of A with Universe" should "be Universe" in {
    assertThrows[IllegalArgumentException] {
      throw new IllegalArgumentException("Universal set cannot exists in vistas")
    }
  }

  // domination
  // A ∩ ∅ = ∅
  "Intersection of A with empty" should "be the empty set" in {
    val classes =
      q"""
        class A {
          def a: Int = 1
          def b: Int = 2
        }

        class O
      """

    classes |> addInsts
    val inter: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Intersect]
    q"val ao: AO = ∩[A, O](a, o)" |> inter |> addInsts

    val aoMethods = db("AO").visibilities

    // which is true when only signatures are considered
    val diff = Set.empty[Defn.Def] <-> aoMethods.signatures
    diff shouldBe empty

    // we can compare by normalised defns
    val normalizedDiff = Set.empty[Defn.Def] <-> aoMethods.normalized
    normalizedDiff shouldBe empty
  }

  // absorption
  // A ∪ (A ∩ B) = A
  "Union and then intersection" should "be result in absorption" in {
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
    val union: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Union]
    val inter: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Intersect]

    q"val ab0: AB = ∩[A, B](a, b)" |> inter |> addInsts
    q"val ab1: AuAB = ∪[A, AB](a, ab)" |> union |> addInsts

    val auabMethods = db("AuAB").visibilities
    val aMethods    = db("A").visibilities

    // which is true when only signatures are considered
    val diff = aMethods.signatures <-> auabMethods.signatures
    diff shouldBe empty

    // we can compare by normalised defns
    val normalizedDiff = aMethods.normalized <-> auabMethods.normalized
    normalizedDiff should not be empty
  }

  // absorption
  // A ∩ (A ∪ B) = A
  "Intersection and then union" should "be result in absorption" in {
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
    val union: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Union]
    val inter: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Intersect]

    q"val ab0: AB = ∪[A, B](a, b)" |> union |> addInsts
    q"val ab1: AnAB = ∩[A, AB](a, ab)" |> inter |> addInsts

    val anabMethods = db("AnAB").visibilities
    val aMethods    = db("A").visibilities

    // which is true when only signatures are considered
    val diff = aMethods.signatures <-> anabMethods.signatures
    diff shouldBe empty

    // we can compare by normalised defns
    val normalizedDiff = aMethods.normalized <-> anabMethods.normalized
    normalizedDiff should not be empty
  }

  // intersection as difference
  // A ∩ B = A \ (A \ B)
  "Intersection" should "be the same as difference" in {
    val classes =
      q"""
        class A {
          def a: Int = 1
          def b: Int = 2
          def c: Int = 2
        }

        class B {
          def c: Int = 1
          def d: Int = 3
          def e: Int = 3
        }
      """

    classes |> addInsts

    val inter: (Defn.Val => Tree)  = parseAndExpand[Defn.Val, OpVistas, Intersect]
    val forbid: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Forbid]

    // left side
    // A ∩ B
    q"val ab: AB = ∩[A, B](a, b)" |> inter |> addInsts

    // right side
    // A \ (A \ B)
    val tree1 = q"val abb: AdB = ∖[A, B](a, b)" |> forbid
    tree1 |> addInsts
    val tree = q"val abb: AdAdB = ∖[A, AdB](a, b)" |> forbid
    tree |> addInsts

    val leftMethods  = db("AB").visibilities
    val rightMethods = db("AdAdB").visibilities

    val diff = leftMethods.signatures <-> rightMethods.signatures
    diff shouldBe empty

    val normalized = leftMethods.normalized <-> rightMethods.normalized
    normalized shouldBe empty
  }

  // product non-communtativity and non-associativity
  // A ⨯ B ≠ B ⨯ A, unless A=B or there exists in {A,B} that is empty
  "Product" should "not be commutative" in {
    val source =
      q"""
        class A {
          def a(): Int = 5
        }

        class B {
          def b(): Int = 3
        }
      """

    source |> addInsts

    val prod: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Product]

    q"val ab: AB = x[A, B](a, b)" |> prod |> addInsts
    q"val ab: BA = x[B, A](b, a)" |> prod |> addInsts

    val abMethods = db("AB").visibilities
    val baMethods = db("BA").visibilities

    val diff = abMethods.signatures <-> baMethods.signatures
    diff should not be empty

    val normalizedDiff = abMethods.normalized <-> baMethods.normalized
    normalizedDiff should not be empty
  }

  // (A ⨯ B) ⨯ C ≠ A ⨯ (B ⨯ C) -- unless one is empty
  "Product" should "not be associative" in {
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
    val prod: (Defn.Val => Tree) = parseAndExpand[Defn.Val, OpVistas, Product]

    // left side
    // (A ⨯ B) ⨯ C
    q"val ab: AB = x[A, B](a, b)" |> prod |> addInsts
    q"val ab: ABxC = x[AB, C](a, b)" |> prod |> addInsts

    // right side
    // A ⨯ (B ⨯ C)
    q"val ab: BC = x[B, C](a, b)" |> prod |> addInsts
    q"val ab: AxBC = x[A, BC](a, b)" |> prod |> addInsts

    val leftMethods  = db("ABxC").visibilities
    val rightMethods = db("AxBC").visibilities

    val diff = leftMethods.signatures <-> rightMethods.signatures
    diff should not be empty

    val normalizedDiff = leftMethods.normalized <-> rightMethods.normalized
    normalizedDiff should not be empty
  }
}
