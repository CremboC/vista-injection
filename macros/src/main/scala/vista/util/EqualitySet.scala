package vista.util

import org.scalactic.Equality

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ListBuffer
import scala.collection.{SetLike, mutable}

/**
  * we extend Set[T] to provide the Set-like interface
  * we extends SetLike[T, EqualitySet[T]] to specify that Set methods will return
  *   instances of type EqualitySet (and not simply Set)
  * [[https://stackoverflow.com/questions/12739432/how-to-implement-a-set-with-a-user-defined-equality Source ]]
  * @author Eric
  */
trait EqualitySet[T] extends Set[T] with SetLike[T, EqualitySet[T]] { outer =>

  /** we need to provide an Equals[T] instance to create an EqualitySet[T] */
  implicit def equality: Equality[T]

  /** our internal implementation as a list of elements */
  protected val set: ListBuffer[T] = ListBuffer[T]()

  /** we need to implements those 4 methods */
  def contains(t: T): Boolean = set.exists(equality.areEqual(_, t))

  def +(t: T): EqualitySet[T] = {
    if (contains(t)) {
      val (_, index) = set.zipWithIndex.find {
        case (el, _) => equality.areEqual(el, t)
      }.get
      set.remove(index)
      set += t
    } else {
      set += t
    }
    this
  }

  def -(t: T): EqualitySet[T] = {
    set -= t
    this
  }

  def iterator: Iterator[T] = set.iterator

  /** we must be able to provide an empty set with the proper equality definition */
  override def empty = new EqualitySet[T] {
    override def equality: Equality[T] = outer.equality
  }
}

/**
  * Companion object for the EqualitySet class
  */
object EqualitySet {

  /**
    * this implicit is absolutely necessary to be able to preserve the resulting
    * collection type when calling `filter`
    */
  implicit def canBuildFrom[T] = new CanBuildFrom[EqualitySet[T], T, EqualitySet[T]] {
    def apply(from: EqualitySet[T]): mutable.Builder[T, EqualitySet[T]] =
      new mutable.Builder[T, EqualitySet[T]] {
        // use a ListBuffer internally to accumulate elements
        private val elems = ListBuffer[T]()
        def +=(t: T): this.type = {
          if (!elems.exists(from.equality.areEqual(_, t))) elems += t
          this
        }
        def clear(): Unit = elems.clear

        // when we finish building the collection
        // we can return an EqualitySet with the original equality relation
        def result() = new EqualitySet[T] {
          override val set: ListBuffer[T]    = elems
          override def equality: Equality[T] = from.equality
        }
      }
    def apply(): mutable.Builder[T, EqualitySet[T]] =
      sys.error("this can't be implemented, because no equality instance is provided")
  }

  /** @return an EqualitySet for a type T having an Equals instance */
  def apply[T: Equality](ts: T*) = {
    var set = new EqualitySet[T] {
      def equality: Equality[T] = implicitly[Equality[T]]
    }.empty
    ts.foreach { t =>
      set += t
    }
    set
  }

  /** @return an EqualitySet for a type T having an Equals instance */
  def apply[T: Equality](ts: Iterable[T]) = {
    var set = new EqualitySet[T] {
      def equality: Equality[T] = implicitly[Equality[T]]
    }.empty
    ts.foreach { t =>
      set += t
    }
    set
  }
}
