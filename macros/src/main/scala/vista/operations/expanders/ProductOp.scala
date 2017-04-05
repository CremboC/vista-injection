package vista.operations.expanders

import vista.operations.parsers.{OpInput, OpVistas}
import vista.semantics
import vista.semantics.Inst
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
            val newldefparams = extractParams(ldef.paramss.head, ltparams, start = 0)

            // generate the list of params for the right def
            val newrdefparams =
              extractParams(rdef.paramss.head, rtparams, start = newldefparams.size)

            // merge into two separate param lists
            val paramss = Seq(newldefparams, newrdefparams)

            val leftArgument = {
              val first = generateTerm(newldefparams, ldef.name)
              val inter = insertTypesToTerm(ltparams.values.to[Seq], first)
              appendSupers(ldef, inter, lclazz)
            }
            val rightArgument = {
              val first = generateTerm(newrdefparams, rdef.name)
              val inter = insertTypesToTerm(rtparams.values.to[Seq], first)
              appendSupers(rdef, inter, rclazz)
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
          trait $traitName extends $leftTypeCtor with $rightTypeCtor {
            ..${pairDefs ++ common.sortBy(_.name.value)}
          }
      """
    }

    private def appendSupers(source: Defn.Def, inter: Term, clazz: Inst): Term = {
      val term = if (hasParenthesis(source, clazz)) {
        val suffix = if (inter.syntax.matches("""^.+\(.+\)$""")) "" else "()"
        s"super[${clazz.name}].${inter.syntax}$suffix".parse[Stat].get
      } else {
        val stripped = inter.syntax.stripSuffix("()")
        s"super[${clazz.name}].$stripped".parse[Stat].get
      }
      term.asInstanceOf[Term]
    }

    private def generateTParams(tparams: Seq[Type.Param]): Map[String, Type.Param] =
      tparams.map(t => (t.name.value, t.copy(name = Type.fresh("Tvista")))).toMap

    private def extractParams(params: Seq[Term.Param],
                              tparams: Map[String, Type.Param],
                              start: Int): Seq[Term.Param] = {
      var current = start
      params.map { param =>
        param.decltpe match {
          case None => param
          case Some(typ) =>
            tparams.get(typ.syntax) match {
              case None => param
              case Some(newtype) =>
                current += 1
                param.copy(name = Term.Name(s"p$current"),
                           decltpe = Option(Type.Name(newtype.name.value)))
            }
        }
      }
    }

    // builds a term-apply-like looking term, depending on number of parameters
    private def generateTerm(params: Seq[Term.Param], seed: Term.Name): Term =
      if (params.isEmpty) seed
      else {
        val ps = params.foldLeft(Seq.empty[Term.Arg]) {
          case (current, t) =>
            t match {
              case t: Term.Param => current :+ t.name.asTerm
            }
        }
        q"$seed(..$ps)"
      }

    // adds generic type parameters if necessary
    private def insertTypesToTerm(params: Seq[Type.Param], term: Term): Term =
      if (params.isEmpty) term
      else {
        val q"$name(..$args)" = term
        val nparams           = params.map(p => s"${p.name.value}".parse[Type].get)

        q"$name[..$nparams](..$args)"
      }

    // checks whether the provide defn originally had parenthesis or not
    private def hasParenthesis(source: Defn.Def, clazz: Inst): Boolean = {
      val defn = clazz.membersWithParents
        .find {
          case d: Defn.Def if d.signature isEqual source.signature => true
          case _                                                   => false
        }
        .getOrElse(abort("Looking for a method which is not present in this class"))

      defn match {
        case q"..$_ def $_[..$_]: $_ = $_" => false
        case _                             => true
      }
    }
  }
}
