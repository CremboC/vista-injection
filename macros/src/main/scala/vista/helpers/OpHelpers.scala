package vista.helpers

import vista.Constants
import vista.util.Extractable._

import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object OpHelpers {
  private final val opVistasR = """.\[.+ & .+ ~> .+\]""".r
  private final val overloadR = """.\[[^&]+ ~> .+\]""".r

  def extractor(char: String): Extractable = (arg: Tree) => arg.syntax.contains(char)
  val Forbid: Extractable                  = extractor(Constants.Forbid)
  val Intersect: Extractable               = extractor(Constants.Intersect)
  val Union: Extractable                   = extractor(Constants.Union)
  val Product: Extractable                 = extractor(Constants.Product)

  val OpVistas: Extractable   = (arg: Tree) => opVistasR.findFirstMatchIn(arg.syntax).isDefined
  val OpOverload: Extractable = (arg: Tree) => overloadR.findFirstMatchIn(arg.syntax).isDefined

  def isUnion(expr: Tree): Boolean     = Union.asPartial(expr)
  def isForbid(expr: Tree): Boolean    = Forbid.asPartial(expr)
  def isIntersect(expr: Tree): Boolean = Intersect.asPartial(expr)
  def isProduct(expr: Tree): Boolean   = Product.asPartial(expr)

  def isOpVistas(tree: Tree): Boolean   = OpVistas.asPartial(tree)
  def isOpOverload(tree: Tree): Boolean = OpOverload.asPartial(tree)

  def hasOp(expr: Tree): Boolean =
    isUnion(expr) || isForbid(expr) || isIntersect(expr) || isProduct(expr)

  def hasCaseMod(cls: Defn.Class): Boolean = cls.mods.exists(_.is[Mod.Case])
}
