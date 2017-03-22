package vista.meta

import scala.collection.immutable.Seq
import scala.meta.Ctor

/**
  * Created by Crembo on 2017-03-21.
  */
trait XCtor {
  object XCtor {
    @inline
    def default: Ctor.Primary =
      Ctor.Primary(Seq.empty, Ctor.Name("this"), Seq.empty)
  }
}
