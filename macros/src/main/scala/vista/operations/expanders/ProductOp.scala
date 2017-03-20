package vista.operations.expanders

import vista.operations.parsers.OpVistas
import vista.semantics
import meta.XDefnIterable
import meta.XMetaIterable

import scala.meta._
import scala.meta.contrib._
import scala.collection.immutable.Seq
import scala.collection.immutable.Map

/**
  * @author Paulius Imbrasas
  */
private[operations] object ProductOp {
  type Product = Op[ProductOp.type]

  private implicit val db = semantics.Database

  val expander: Expander[OpVistas, Product] = (inp: OpVistas) => {
    val lsignatures = db(inp.lclass).methods.signatures
    val rsignatures = db(inp.rclass).methods.signatures

    val pairs = lsignatures >< rsignatures

    val pairDefs = pairs.map {
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
        val nleft = insertTypesToTerm(ltparams.values.to[Seq], leftTerm)

        val rightTerm = generateTerm(newrdefparams, q"${rdef.name}()")
        val nright = insertTypesToTerm(rtparams.values.to[Seq], rightTerm)

        Defn.Def(
          mods = Seq.empty,
          name = name,
          tparams = (ltparams.values ++ rtparams.values).to[Seq],
          paramss = paramss,
          decltpe = None,
          body = Term.Tuple(nleft :: nright :: Nil)
        )
    }.to[Seq]

    val traitName = Type.Name(inp.newtype)

    val leftTypeCtor = Ctor.Name(inp.lclass)
    val rightTypeCtor = Ctor.Name(inp.rclass)

    // TODO: common methods?

    inp.newvar match {
      case None => ???
      case Some(nvar) =>
        q"""
            trait $traitName extends $leftTypeCtor with $rightTypeCtor {
              ..$pairDefs
            }
            val ${Term.Name(nvar).asPat} = new ${traitName.asCtorRef} {}
          """
    }
  }

  private def generateTParams(tparams: Seq[Type.Param]): Map[String, Type.Param] =
    tparams.map(t => (t.name.value, t.copy(name = Type.fresh("Tvista")))).toMap

  private def extractParams(params: Seq[Term.Param], tparams: Map[String, Type.Param]): Seq[Term.Param] =
    params.map { param =>
      param.decltpe match {
        case None => param
        case Some(typ) =>
          tparams.get(typ.syntax) match {
            case None => param
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
      val nparams = params.map(p => s"${p.name.value}".parse[Type].get)

      q"$name[..$nparams](..$args)"
    }
}
