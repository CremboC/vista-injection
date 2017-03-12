import org.scalactic.Equality

import scala.meta._
import scala.meta.contrib._

/**
  * @author Paulius Imbrasas
  */
package object vista {
  implicit val treeStructureEquality =
    new Equality[Tree] {
      def areEqual(a: Tree, b: Any): Boolean =
        b match {
          case bt: Tree => a.isEqual(bt)
          case _ => false
        }
    }

  implicit val defnStructureEquality =
    new Equality[Defn.Def] {
      def areEqual(a: Defn.Def, b: Any): Boolean =
        b match {
          case bt: Defn.Def => a.isEqual(bt)
          case _ => false
        }
    }

}
