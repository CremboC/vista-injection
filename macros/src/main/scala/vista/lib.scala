package vista

/**
  * Created by Crembo on 2017-03-29.
  */
object lib extends {
  type ~>[A, B] = (A, B)
  type &[A, B]  = (A, B)

  trait AnyV

  sealed trait Vista extends AnyV
  trait Difference   extends Vista
  trait Intersection extends Vista
  trait Product      extends Vista
  trait Union        extends Vista

  // union
  def ∪[A <: ~>[&[_, _], _]](left: Any, right: Any): Any =
    throw new RuntimeException("Should be compiled out")

  // intersection
  def ∩[A <: ~>[&[_, _], _]](left: Any, right: Any): Any =
    throw new RuntimeException("Should be compiled out")

  // difference
  def ∖[A <: ~>[_, _]](left: Any, arg: Any): Any =
    throw new RuntimeException("Should be compiled out")

  // product
  def ⨯[A <: ~>[&[_, _], _]](left: Any, right: Any): Any =
    throw new RuntimeException("Should be compiled out")
}
