package vista.util.meta

import scala.collection.immutable.Seq
import scala.meta.Ctor

trait XCtor {
  object XCtor {
    @inline
    def default: Ctor.Primary =
      Ctor.Primary(Seq.empty, Ctor.Name("this"), Seq.empty)
  }
}
