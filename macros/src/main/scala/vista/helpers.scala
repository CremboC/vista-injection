import scala.meta._

/**
  * @author Paulius Imbrasas
  */
package object helpers {

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
