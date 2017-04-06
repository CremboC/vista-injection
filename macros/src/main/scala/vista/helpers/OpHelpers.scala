package vista.helpers

import vista.Constants
import vista.util.Extractable._

import scala.meta._

object OpHelpers {

  def extractor(char: String): Extractable1[Term.Apply] = new Extractable1[Term.Apply] {
    override def unapply[A <: Tree](arg: A): Option[Term.Apply] = arg match {
      case arg: Term.Apply if arg.syntax.startsWith(char) => Option(arg)
      case _                                              => None
    }
  }

  val Forbid: Extractable1[Term.Apply]    = extractor(Constants.Forbid)
  val Intersect: Extractable1[Term.Apply] = extractor(Constants.Intersect)
  val Union: Extractable1[Term.Apply]     = extractor(Constants.Union)
  val Product: Extractable1[Term.Apply]   = extractor(Constants.Product)

  val Subset = new Extractable1[Term] {
    override def unapply[A <: Tree](arg: A): Option[Term] = arg match {
      case arg: Term.Apply if arg.syntax.contains(Constants.Subset)      => Option(arg)
      case arg: Term.ApplyInfix if arg.syntax.contains(Constants.Subset) => Option(arg)
      case _                                                             => None
    }
  }

  val OpVistas: Extractable = new Extractable {
    override def unapply(arg: Tree): Boolean = arg match {
      case q"$_[$_, $_, $_]($_, $_)" => true
      case _                         => false
    }
  }

  val OpOverload: Extractable = new Extractable {
    override def unapply(arg: Tree): Boolean = arg match {
      case q"$_[$_, $_]($_, $_)" => true
      case _                     => false
    }
  }

  val HasOp: Extractable1[Term.Apply] = new Extractable1[Term.Apply] {
    override def unapply[A <: Tree](arg: A): Option[Term.Apply] = arg match {
      case arg: Term.Apply if hasOp(arg) => Option(arg)
      case _                             => None
    }
  }

  def isUnion(expr: Tree): Boolean     = Union.asPartial(expr)
  def isForbid(expr: Tree): Boolean    = Forbid.asPartial(expr)
  def isIntersect(expr: Tree): Boolean = Intersect.asPartial(expr)
  def isProduct(expr: Tree): Boolean   = Product.asPartial(expr)

  def hasOp(expr: Tree): Boolean =
    isUnion(expr) || isForbid(expr) || isIntersect(expr) || isProduct(expr)

  def hasCaseMod(cls: Defn.Class): Boolean = cls.mods.exists(_.is[Mod.Case])
}
