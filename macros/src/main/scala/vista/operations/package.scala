package vista

import vista.operations.expanders.{Expander, Op}
import vista.operations.parsers.{OpInput, Parser}

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
package object operations {

  def expand[A <: OpInput, B <: Op[_]](input: A)(implicit expander: Expander[A, B]): Term.Block =
    expander.expand(input)

  def parse[From, To <: OpInput](defn: From)(implicit parser: Parser[From, To]): Option[To] =
    parser.parse(defn)

  def parseAndExpand[Source, Input <: OpInput, Result <: Op[_]](defn: Source)
                                                      (implicit parser: Parser[Source, Input], expander: Expander[Input, Result]): Term.Block =
    parse[Source, Input](defn).map(d => expand[Input, Result](d)).getOrElse {
      throw new IllegalArgumentException
    }
}
