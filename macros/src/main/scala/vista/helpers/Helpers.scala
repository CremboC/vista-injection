package vista.helpers

import scala.meta.Tree
import scala.meta.transversers.Transformer

/**
  * @author paulius
  */
object Helpers {
  implicit class NonRecursiveTransformer(tree: Tree) {
    def transformNR(fn: PartialFunction[Tree, Tree]): Tree = {
      object transformer extends Transformer {
        override def apply(tree: Tree): Tree = {
          if (fn.isDefinedAt(tree)) fn(tree)
          else super.apply(tree)
        }
      }
      transformer(tree)
    }
  }
}
