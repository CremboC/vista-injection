package vista.util.meta

import scala.collection.immutable.Seq
import scala.meta._

/**
  * Created by Crembo on 2017-04-06.
  */
trait XTermParamSeq {
  implicit class XTermParamSeq(self: Seq[Seq[Term.Param]]) {
    def asTermArg: Seq[Seq[Term]] =
      self.map(_.map { param =>
        Term.Name(param.name.value)
      })
  }
}
