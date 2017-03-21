package vistas

trait AnyV
sealed trait Vista extends AnyV

trait Union        extends Vista
trait Intersection extends Vista
trait Difference   extends Vista
trait Product      extends Vista

object Vista {

  implicit class ops[A](self: A) {
    def ∪[B](other: B) = throw new RuntimeException("Should be compiled out")
    def ∩[B](right: B) = throw new RuntimeException("Should be compiled out")
    def ∖(arg: {})    = throw new RuntimeException("Should be compiled out")
    def ⨯[B](right: B) = throw new RuntimeException("Should be compiled out")
  }

  // union
  def ∪[A, B](left: A, right: B): Any = throw new RuntimeException("Should be compiled out")

  // intersection
  def ∩[A, B](left: A, right: B): Any = throw new RuntimeException("Should be compiled out")

  // difference
  def ∖[A](left: A, arg: Any): Any = throw new RuntimeException("Should be compiled out")

  // product
  def ⨯[A, B](left: A, right: B): Any = throw new RuntimeException("Should be compiled out")
}
