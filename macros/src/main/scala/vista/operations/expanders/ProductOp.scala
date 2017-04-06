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
    override def expand(inp: OpVistas): Defn.Trait = {
      val leftClass  = db(inp.lclass)
      val rightClass = db(inp.rclass)

      val leftMethods  = leftClass.visibilities
      val rightMethods = rightClass.visibilities

      // go through left defs
      // f ~~> has new term
      //   go through f defs
      //   g ~~> no new term
      //      go through right defs
      //      a ~~> has new term
      //        go through a defs
      //        b ~~> no new term
      //          build body (left.f.g) -> (right.a.b)

      import shapeless._

      object L
      object R
      type LR = L.type :+: R.type :+: CNil
      case class DefnPair(defn: Defn.Def, sig: Defn.Def, source: LR)

      def processLeftMethod(ldef: Defn.Def, stack: List[DefnPair])(implicit cntr: Counter =
                                                                     new Counter()): Defn.Def = {
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

      def processRightMethod(rdef: Defn.Def, stack: List[DefnPair])(
          implicit cntr: Counter): Defn.Def = {
        val rsig = rdef.signature
        val elem = DefnPair(rdef, rsig, Coproduct[LR](R))

        if (rdef.body.is[Term.New]) {
          val defns = rdef.body.asInstanceOf[Term.New].templ.extract[Defn.Def]
          val processed =
            defns
              .map(m => processLeftMethod(m, elem :: stack))
              .to[Seq]

          rdef.copy(body = q"new { ..$processed }",
                    decltpe = None,
                    paramss = if (rdef.hasParenthesis) rsig.paramss else rdef.paramss)
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
                    paramss = if (rdef.hasParenthesis) rsig.paramss else rdef.paramss)
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

//
//      if (leftSignatures.exists(_.hasMultiParamList)
//          || rightSignatures.exists(_.hasMultiParamList))
//        abort(
//          "Product only supports classes which do not contain methods with multiple parameter lists.")
//
//      val pairs = leftSignatures >< rightSignatures
//
//      val pairDefs = pairs
//        .map {
//          case (ldef, rdef) =>
//            // create new def name
//            val name = Term.Name(s"${ldef.name.value}${rdef.name.value}")
//
//            // regenerate all generic params
//            val ltparams = generateTParams(ldef.tparams)
//            val rtparams = generateTParams(rdef.tparams)
//
//            // generate the list of params for the left def
//            val newldefparams = extractParams(ldef.paramss.head, ltparams, start = 0)
//
//            // generate the list of params for the right def
//            val newrdefparams =
//              extractParams(rdef.paramss.head, rtparams, start = newldefparams.size)
//
//            // merge into two separate param lists
//            val paramss = Seq(newldefparams, newrdefparams)
//
//            val leftArgument = {
//              val first = generateTerm(newldefparams, ldef.name)
//              val inter = insertTypesToTerm(ltparams.values.to[Seq], first)
//              appendSupers(ldef, inter, leftClass)
//            }
//            val rightArgument = {
//              val first = generateTerm(newrdefparams, rdef.name)
//              val inter = insertTypesToTerm(rtparams.values.to[Seq], first)
//              appendSupers(rdef, inter, rightClass)
//            }
//
//            Defn.Def(
//              mods = Seq.empty,
//              name = name,
//              tparams = (ltparams.values ++ rtparams.values).to[Seq],
//              paramss = paramss,
//              decltpe = None,
//              body = Term.Tuple(leftArgument :: rightArgument :: Nil)
//            )
//        }
//        .to[Seq]
//
//      val traitName = Type.Name(inp.newtype)
//
//      val leftTypeCtor  = db.ctor(inp.lclass)
//      val rightTypeCtor = db.ctor(inp.rclass)
//
//      val common = commonMethods(inp).to[Seq]
//
//      q"""
//          trait $traitName extends $leftTypeCtor with $rightTypeCtor {
//            ..${pairDefs ++ common.sortBy(_.name.value)}
//          }
//      """
    }

//    private def appendSupers(source: Defn.Def, inter: Term, clazz: Inst): Term = {
//      val term = if (hasParenthesis(source, clazz)) {
//        val suffix = if (inter.syntax.matches("""^.+\(.+\)$""")) "" else "()"
//        s"super[${clazz.name}].${inter.syntax}$suffix".parse[Stat].get
//      } else {
//        val stripped = inter.syntax.stripSuffix("()")
//        s"super[${clazz.name}].$stripped".parse[Stat].get
//      }
//      term.asInstanceOf[Term]
//    }
//
//    private def generateTParams(tparams: Seq[Type.Param]): Map[String, Type.Param] =
//      tparams.map(t => (t.name.value, t.copy(name = Type.fresh("Tvista")))).toMap
//
//    private def extractParams(params: Seq[Term.Param],
//                              tparams: Map[String, Type.Param],
//                              start: Int): Seq[Term.Param] = {
//      var current = start
//      params.map { param =>
//        param.decltpe match {
//          case None => param
//          case Some(typ) =>
//            tparams.get(typ.syntax) match {
//              case None => param
//              case Some(newtype) =>
//                current += 1
//                param.copy(name = Term.Name(s"p$current"),
//                           decltpe = Option(Type.Name(newtype.name.value)))
//            }
//        }
//      }
//    }
//
//    // builds a term-apply-like looking term, depending on number of parameters
//    private def generateTerm(params: Seq[Term.Param], seed: Term.Name): Term =
//      if (params.isEmpty) seed
//      else {
//        val ps = params.foldLeft(Seq.empty[Term.Arg]) {
//          case (current, t) =>
//            t match {
//              case t: Term.Param => current :+ t.name.asTerm
//            }
//        }
//        q"$seed(..$ps)"
//      }
//
//    // adds generic type parameters if necessary
//    private def insertTypesToTerm(params: Seq[Type.Param], term: Term): Term =
//      if (params.isEmpty) term
//      else {
//        val q"$name(..$args)" = term
//        val nparams           = params.map(p => s"${p.name.value}".parse[Type].get)
//
//        q"$name[..$nparams](..$args)"
//      }
//
//    // checks whether the provide defn originally had parenthesis or not
//    private def hasParenthesis(source: Defn.Def, clazz: Inst): Boolean = {
//      val defn = clazz.membersWithParents
//        .find {
//          case d: Defn.Def if d.signature isEqual source.signature => true
//          case _                                                   => false
//        }
//        .getOrElse(abort("Looking for a method which is not present in this class"))
//
//      defn match {
//        case q"..$_ def $_[..$_]: $_ = $_" => false
//        case _                             => true
//      }
//    }
  }
}
