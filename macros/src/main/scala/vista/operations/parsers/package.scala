package vista.operations

import scala.meta.Defn

/**
  * @author Paulius Imbrasas
  */
package object parsers {
  trait Parser[From, To] {
    def parse(defn: From): Option[To]
  }

  implicit val unionizeParser: Parser[Defn.Val, UnionizeInput] = UnionizeParser.defnValToInput
  implicit val intersectParser: Parser[Defn.Val, IntersectInput] = IntersectParser.defnValToInput

  implicit val forbidParser: Parser[Defn.Val, ForbidInput] = ForbidParser.defnValToInput
  implicit val forbidParser2: Parser[Defn.Def, ForbidInput] = ForbidParser.defnDefToInput

  implicit val productParser: Parser[Defn.Val, ProductInput] = ProductParser.defnValToInput

  def parse[From, To](defn: From)(implicit parser: Parser[From, To]): Option[To] = parser.parse(defn)
}
