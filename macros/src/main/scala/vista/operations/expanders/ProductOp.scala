package vista.operations.expanders

import vista.operations.parsers.OpVistas
import vista.semantics
import meta.XDefnIterable
import meta.XMetaIterable

import scala.meta._
import scala.meta.contrib._
import scala.collection.immutable.Seq

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

    def generateTParams(tparams: Seq[Type.Param]): Map[String, Type.Param] =
      tparams.map(t => (t.name.value, t.copy(name = Type.fresh("Tvista")))).toMap

    val pairDefs = pairs.map {
      case (ldef, rdef) =>
        // create new def name
        val name = Term.Name(s"${ldef.name.value}${rdef.name.value}")

        // regenerate all generic params
        val tparams = generateTParams(ldef.tparams) ++ generateTParams(rdef.tparams)

        // generate the list of params for the left def
        val newldefparams = ldef.paramss.head.map { param =>
          param.decltpe match {
            case None => param
            case Some(typ) =>
              tparams.get(typ.syntax) match {
                case None => param
                case Some(newtype) => param.copy(decltpe = Option(Type.Name(newtype.name.value)))
              }
          }
        }

        // generate the list of params for the right def
        val newrdefparams = rdef.paramss.head.map { param =>
          param.decltpe match {
            case None => param
            case Some(typ) =>
              tparams.get(typ.syntax) match {
                case None => param
                case Some(newtype) => param.copy(decltpe = Option(Type.Name(newtype.name.value)))
              }
          }
        }

        // merge into two separate param lists
        val paramss = Seq(newldefparams, newrdefparams)

        // generate the body of the product def from the left
        val bodyLeft = newldefparams.foldLeft(q"(${ldef.name}(), ${rdef.name}())") {
          case (body, current) =>
            val left :: right :: Nil = body.args
            val lfunc = left.asInstanceOf[Term.Apply]
            val currentArg = current.name.asTerm
            val nleft = lfunc.copy(args = lfunc.args :+ currentArg)

            body.copy(args = nleft :: right :: Nil)
        }

        // generate the body of the product def from the right
        val body = newrdefparams.foldLeft(bodyLeft) {
          case (body, current) =>
            val left :: right :: Nil = body.args
            val rfunc = right.asInstanceOf[Term.Apply]
            val currentArg = current.name.asTerm
            val nright = rfunc.copy(args = rfunc.args :+ currentArg)

            body.copy(args = left :: nright :: Nil)
        }

        // merge bodies
//        val left :: _ :: Nil = bodyLeft.args
//        val _ :: right :: Nil = bodyRight.args
//        val body = Term.Tuple(args = left :: right :: Nil)

        Defn.Def(Seq.empty, name,
          Seq.empty, paramss, None, body = body)
    }.toSeq.asInstanceOf[Seq[Stat]]

    val traitName = Type.Name(inp.newtype)

    val leftTypeCtor = Ctor.Name(inp.lclass)
    val rightTypeCtor = Ctor.Name(inp.rclass)

    // TODO: common methods

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
}
