package vista.operations.expanders

import vista.operations.parsers.{OpInput, OpVistas}
import vista.semantics
import vista.semantics.Inst
import vista.util.Counter
import vista.util.meta.xtensions._

import scala.collection.immutable.{Map, Seq}
import scala.meta._

/**
  * @author Paulius Imbrasas
  */
object ProductOp {
  type Product = Op[ProductOp.type]

  private val db = semantics.Database

  implicit object ProductCtor extends Constructable[Product] {
    override def members(input: OpInput): Seq[Defn] = input match {
      case input: OpVistas =>
        ctorMembersDefns(db(input.lclass), input.lvar) ++
          ctorMembersDefns(db(input.rclass), input.rvar)
      case _ => abort("Product is cannot be overridden")
    }
  }

  implicit object VistaExpander extends Expander[OpVistas, Product] {
    override def expand(inp: OpVistas): Defn.Trait = {
      val leftClass  = db(inp.lclass)
      val rightClass = db(inp.rclass)

      implicit val paramCounter = new Counter[Defn.Def]()
      implicit val typeCounter  = new Counter[Type]()

      val leftSignatures  = leftClass.visibilities.map(m => m.signature  -> m)
      val rightSignatures = rightClass.visibilities.map(m => m.signature -> m)

      val pairs = for (l <- leftSignatures; r <- rightSignatures) yield (l, r)

      val pairDefs = pairs
        .map {
          case ((lsig, ldef), (rsig, rdef)) =>
            // create new def name
            val name = Term.Name(s"${lsig.name.value}${rsig.name.value}")

            // regenerate all generic params
            val ltparams = generateTParams(lsig.tparams)
            val rtparams = generateTParams(rsig.tparams)

            // generate the list of params for the left def
            val newldefparams = extractParams(lsig.paramss, ltparams, 0)

            // generate the list of params for the right def
            val newrdefparams = extractParams(rsig.paramss, rtparams, newldefparams.size)

            // merge into two separate param lists
            val paramss = newldefparams ++ newrdefparams

            val leftArgument = {
              val term = generateTerm(newldefparams, lsig.name)
              appendSupers(ldef, term, leftClass)
            }
            val rightArgument = {
              val term = generateTerm(newrdefparams, rsig.name)
              appendSupers(rdef, term, rightClass)
            }

            Defn.Def(
              mods = Seq.empty,
              name = name,
              tparams = (ltparams.values ++ rtparams.values).to[Seq],
              paramss = paramss,
              decltpe = None,
              body = Term.Tuple(leftArgument :: rightArgument :: Nil)
            )
        }
        .to[Seq]

      val traitName = Type.Name(inp.newtype)

      val leftTypeCtor  = db.ctor(inp.lclass)
      val rightTypeCtor = db.ctor(inp.rclass)

      val common = commonMethods(inp).to[Seq]

      q"""
          @vista.product
          trait $traitName extends $leftTypeCtor with $rightTypeCtor {
            ..${pairDefs ++ common.sortBy(_.name.value)}
          }
      """
    }

    private def appendSupers(defn: Defn.Def, inter: Term, clazz: Inst): Term = {
      val term =
        if (defn.hasParenthesis) {
          val suffix = if (inter.syntax.matches("""^.+\((.+)?\)$""")) "" else "()"
          s"super[${clazz.name}].${inter.syntax}$suffix".parse[Stat].get
        } else {
          val stripped = inter.syntax.stripSuffix("()")
          s"super[${clazz.name}].$stripped".parse[Stat].get
        }
      term.asInstanceOf[Term]
    }

    private def generateTParams(tparams: Seq[Type.Param])(
        implicit cntr: Counter[Type]): Map[String, Type.Param] =
      tparams.map(t => (t.name.value, t.copy(name = Type.Name(s"Tvista${cntr.next}")))).toMap

    private def extractParams(paramss: Seq[Seq[Term.Param]],
                              tparams: Map[String, Type.Param],
                              start: Int): Seq[Seq[Term.Param]] = {
      var current = start
      if (paramss.head.isEmpty) paramss
      else {
        paramss.map {
          _.map { param =>
            val typ = param.decltpe.getOrElse(abort("Definition parameter didn't have a decltpe"))
            tparams
              .get(typ.syntax)
              .map { newtype =>
                current += 1
                param.copy(name = Term.Name(s"p$current"),
                           decltpe = Option(Type.Name(newtype.name.value)))
              }
              .getOrElse(param)
          }
        }
      }
    }

    // builds a term-apply-like looking term, depending on number of parameters
    private def generateTerm(paramss: Seq[Seq[Term.Param]], seed: Term.Name): Term =
      if (paramss.isEmpty) seed
      else {
        q"$seed(...${paramss.asTermArg})"
      }
  }
}
