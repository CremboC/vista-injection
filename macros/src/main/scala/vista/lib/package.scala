package vista

package object lib {
  trait AnyV
  type Vista[A] = A

  case class ForbiddenMethodException(msg: String = "") extends Exception(msg)

  trait Product {
    def invoke[A](args: Any*)(paramss: Any*): (Any, Any) =
      throw new RuntimeException("Should be compiled out")
  }

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
  def ⨯[A, B, T](left: A, right: B): Product =
    throw new RuntimeException("Should be compiled out")
}
