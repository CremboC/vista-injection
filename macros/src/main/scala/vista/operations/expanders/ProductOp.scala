package vista.operations.expanders

import vista.operations.parsers.{OpInput, OpVistas}
import vista.semantics
import vista.util.meta.xtensions._

import scala.collection.immutable.{Map, Seq}
import scala.meta._
import scala.meta.contrib._

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
      val lclazz = db(inp.lclass)
      val rclazz = db(inp.rclass)

      val lsignatures = lclazz.visibilities.signatures
      val rsignatures = rclazz.visibilities.signatures

      val pairs = lsignatures >< rsignatures

      val pairDefs = pairs
        .map {
          case (ldef, rdef) =>
            // create new def name
            val name = Term.Name(s"${ldef.name.value}${rdef.name.value}")

            // regenerate all generic params
            val ltparams = generateTParams(ldef.tparams)
            val rtparams = generateTParams(rdef.tparams)

            // generate the list of params for the left def
            val newldefparams = extractParams(ldef.paramss.head, ltparams)

            // generate the list of params for the right def
            val newrdefparams = extractParams(rdef.paramss.head, rtparams)

            // merge into two separate param lists
            val paramss = Seq(newldefparams, newrdefparams)

            val leftTerm = generateTerm(newldefparams, q"${ldef.name}()")
            val nleft    = insertTypesToTerm(ltparams.values.to[Seq], leftTerm)

            val rightTerm = generateTerm(newrdefparams, q"${rdef.name}()")
            val nright    = insertTypesToTerm(rtparams.values.to[Seq], rightTerm)

            Defn.Def(
              mods = Seq.empty,
              name = name,
              tparams = (ltparams.values ++ rtparams.values).to[Seq],
              paramss = paramss,
              decltpe = None,
              body = Term.Tuple(nleft :: nright :: Nil)
            )
        }
        .to[Seq]

      val traitName = Type.Name(inp.newtype)

      val leftTypeCtor  = db.ctor(inp.lclass)
      val rightTypeCtor = db.ctor(inp.rclass)

      q"""
          trait $traitName extends $leftTypeCtor with $rightTypeCtor {
            ..$pairDefs
          }
      """
    }

    private def generateTParams(tparams: Seq[Type.Param]): Map[String, Type.Param] =
      tparams.map(t => (t.name.value, t.copy(name = Type.fresh("Tvista")))).toMap

    private def extractParams(params: Seq[Term.Param],
                              tparams: Map[String, Type.Param]): Seq[Term.Param] =
      params.map { param =>
        param.decltpe match {
          case None => param
          case Some(typ) =>
            tparams.get(typ.syntax) match {
              case None          => param
              case Some(newtype) => param.copy(decltpe = Option(Type.Name(newtype.name.value)))
            }
        }
      }

    private def generateTerm(params: Seq[Term.Param], seed: Term.Apply): Term.Apply =
      params.foldLeft(seed) {
        case (term, current) => term.copy(args = term.args :+ current.name.asTerm)
      }

    private def insertTypesToTerm(params: Seq[Type.Param], term: Term.Apply): Term.Apply =
      if (params.isEmpty) term
      else {
        val q"$name(..$args)" = term
        val nparams           = params.map(p => s"${p.name.value}".parse[Type].get)

        q"$name[..$nparams](..$args)"
      }
  }
}
