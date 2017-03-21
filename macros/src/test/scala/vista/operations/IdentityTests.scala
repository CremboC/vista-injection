package vista.operations

import org.scalatest._

/**
  * Created by Crembo on 2017-03-20.
  */
class IdentityTests extends FlatSpec with Matchers {
  // commutativity
  // A ∪ B = B ∪ A
  // A ∩ B = B ∩ A

  // associativity
  // (A ∪ B) ∪ C = A ∪ (B ∪ C)
  // (A ∩ B) ∩ C = A ∩ (B ∩ C)

  // distributivity
  // A ∪ (B ∩ C) = (A ∪ B) ∩ (A ∪ C)
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
