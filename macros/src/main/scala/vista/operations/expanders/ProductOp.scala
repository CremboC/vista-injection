package vista.operations.expanders

import vista.operations.parsers.{OpInput, OpVistas}
import vista.semantics
import vista.util.Counter
import vista.util.meta.xtensions._

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.contrib._

/**
  * @author Paulius Imbrasas
  */
object ProductOp {
  type Product = Op[ProductOp.type]

  final val LeftName  = Term.Name("left$vista$type")
  final val RightName = Term.Name("right$vista$type")

  private val db = semantics.Database

  implicit object ProductCtor extends Constructable[Product] {
    override def members(input: OpInput): Seq[Defn] = input match {
      case input: OpVistas =>
        val left =
          q"override val ${LeftName.asPat}: Vista[${Type.Name(input.lclass)}] = ${Term.Name(input.lvar)}"
        val right =
          q"override val ${RightName.asPat}: Vista[${Type.Name(input.rclass)}] = ${Term.Name(input.rvar)}"
        ctorMembersDefns(db(input.lclass), input.lvar) ++
          ctorMembersDefns(db(input.rclass), input.rvar) ++ Seq(left, right)
      case _ => abort("Product is cannot be overridden")
    }
  }

  implicit object VistaExpander extends Expander[OpVistas, Product] {
    import shapeless._

    object L
    object R
    type LR = L.type :+: R.type :+: CNil

    case class DefnPair(defn: Defn.Def, sig: Defn.Def, source: LR)

    override def expand(inp: OpVistas): Defn.Trait = {
      val leftClass  = db(inp.lclass)
      val rightClass = db(inp.rclass)

      val leftMethods  = leftClass.visibilities
      val rightMethods = rightClass.visibilities

      // example walk through of the algorithm
      // go through left defs
      // f ~~> has new term
      //   go through f defs
      //   g ~~> no new term
      //      go through right defs
      //      a ~~> has new term
      //        go through a defs
      //        b ~~> no new term
      //          build body (left.f.g) -> (right.a.b)

      // processes methods which come from the left operand
      def processLeftMethod(ldef: Defn.Def, stack: List[DefnPair])(
          implicit cntr: Counter[Defn.Def] = new Counter()): Defn.Def = {
        val lsig = ldef.signature
        val elem = DefnPair(ldef, lsig, Coproduct[LR](L))

        val defs = if (ldef.body.is[Term.New]) {
          val subdefns = ldef.body.asInstanceOf[Term.New].templ.extract[Defn.Def]
          subdefns.map(m => processLeftMethod(m, elem :: stack)).to[Seq]
        } else {
          rightMethods.map(m => processRightMethod(m, elem :: stack)).to[Seq]
        }

        ldef.copy(body = q"new { ..$defs }",
                  decltpe = None,
                  paramss = if (ldef.hasParenthesis) lsig.paramss else ldef.paramss)
      }

      // processes methods which come from the right operand
      def processRightMethod(rdef: Defn.Def, stack: List[DefnPair])(
          implicit cntr: Counter[Defn.Def]): Defn.Def = {
        val rsig = rdef.signature
        val elem = DefnPair(rdef, rsig, Coproduct[LR](R))

        if (rdef.body.is[Term.New]) {
          val defns = rdef.body.asInstanceOf[Term.New].templ.extract[Defn.Def]
          val processed =
            defns
              .map(m => processRightMethod(m, elem :: stack))
              .to[Seq]

          rdef.copy(body = q"new { ..$processed }",
                    decltpe = None,
                    paramss = if (rdef.hasParenthesis) rsig.paramss else rdef.paramss,
                    mods = Seq.empty)
        } else {

          @tailrec
          def build(seed: Term, stack: List[DefnPair]): Term =
            if (stack.isEmpty) seed
            else {
              val (DefnPair(defn, sig, _) +: rest) = stack
              val stat =
                if (defn.hasParenthesis) q"$seed.${sig.name}(...${sig.paramss.asTermArg})"
                else q"$seed.${defn.name}"
              build(stat, rest)
            }

          val left = {
            val leftSource =
              (elem :: stack).reverse.filter(_.source match {
                case Inl(_) => true
                case _      => false
              })
            build(LeftName, leftSource)
          }

          val right = {
            val rightSource =
              (elem :: stack).reverse.filter(_.source match {
                case Inr(_) => true
                case _      => false
              })
            build(RightName, rightSource)
          }

          val nbody = q"$left -> $right"

          rdef.copy(body = nbody,
                    decltpe = None,
                    paramss = if (rdef.hasParenthesis) rsig.paramss else rdef.paramss,
                    mods = Seq.empty)
        }
      }

      val bodies = leftMethods
        .map(m => processLeftMethod(m, List.empty).copy(mods = Seq(mod"@vista.product.pair")))
        .to[Seq]

      val traitName = Type.Name(inp.newtype)
      q"""
          trait $traitName {
            val ${LeftName.asPat}: Vista[${Type.Name(inp.lclass)}]
            val ${RightName.asPat}: Vista[${Type.Name(inp.rclass)}]

            ..$bodies
          }
      """
    }
  }
}
