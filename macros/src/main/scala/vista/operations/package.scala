package vista

import vista.operations.expanders.{Expander, Op}
import vista.operations.parsers.{OpInput, Parser}

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
package object operations {
  def parseAndExpand[A <: Tree, B <: OpInput, C <: Op[_]](
      defn: A)(implicit parser: Parser[A, B], expander: Expander[B, C]): Defn.Trait = {
    parser.parse(defn).map(d => expander.expand(d)).getOrElse {
      throw new IllegalArgumentException
    }
  }

}
