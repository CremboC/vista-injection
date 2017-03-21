package meta

import scala.collection.immutable.Seq
import scala.meta._

/**
  * Created by Crembo on 2017-03-21.
  */
trait XDefn {

  /**
    * Defn.Def extensions.
    * @param defn
    */
  implicit class XDefn(defn: Defn.Def) {
    def signature: Defn.Def = {
      // ensures that def g() === def g
      val paramss = if (defn.paramss.isEmpty) Seq(Seq.empty) else defn.paramss
      defn.copy(body = Term.Block(Seq.empty), decltpe = None, paramss = paramss)
    }
  }

  implicit class XDefnIterable[A <: Defn.Def](self: Iterable[A]) {
    @inline
    def signatures: Set[Defn.Def] = self.map(_.signature).toSet
  }
}
