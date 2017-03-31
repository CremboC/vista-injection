package vista

package object lib {
  trait AnyV

  // union
  def ∪[A, B, T](left: A, right: B): A with B =
    throw new RuntimeException("Should be compiled out")

  // intersection
  def ∩[A, B, T](left: A, right: B): A with B =
    throw new RuntimeException("Should be compiled out")

  // difference
  def ∖[A, T](left: A, arg: Any): A =
    throw new RuntimeException("Should be compiled out")

  // difference
  def ∖∖[A, B, T](left: A, arg: B): A =
    throw new RuntimeException("Should be compiled out")

  // product
  def ⨯[A, B, T](left: A, right: B): T =
    throw new RuntimeException("Should be compiled out")

  implicit class VistaOpsUnit(val f: Unit) extends AnyVal {
    def ⊆[A](a: A): Boolean =
      throw new RuntimeException("Should be compiled out")
  }

  implicit class VistaOpsRef(val f: AnyRef) extends AnyVal {
    def ⊆[_, A](a: A): Boolean =
      throw new RuntimeException("Should be compiled out")
  }
}
