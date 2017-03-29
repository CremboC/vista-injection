package vista.util

import scala.meta.Tree

/**
  * Created by Crembo on 2017-03-29.
  */
object Extractable {
  trait Extractable {
    def unapply(arg: Tree): Boolean
  }

  implicit class ExtractableOp(ex: Extractable) {
    def asPartial: PartialFunction[Tree, Boolean] = {
      case ex() => true
      case _    => false
    }
  }
}
