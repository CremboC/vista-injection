package vista

import org.scalactic.Equality
import org.scalatest._

import scala.meta._
import scala.meta.contrib._

/**
  * Created by Crembo on 2017-03-22.
  */

sealed trait BaseTest extends Matchers with ResetsDatabase {
  protected val db = semantics.Database
  protected val addInsts: Tree => Unit = _.traverse {
    case c: Defn.Class => db.add(c)
    case c: Defn.Trait => db.add(c)
  }

  implicit val treeStructureEquality =
    new Equality[Tree] {
      def areEqual(a: Tree, b: Any): Boolean =
        b match {
          case bt: Tree => a isEqual bt
          case _        => false
        }
    }

  implicit val defnStructureEquality =
    new Equality[Defn.Def] {
      def areEqual(a: Defn.Def, b: Any): Boolean =
        b match {
          case bt: Defn.Def => a isEqual bt
          case _            => false
        }
    }

  implicit val traitStructureEquality =
    new Equality[Defn.Trait] {
      def areEqual(a: Defn.Trait, b: Any): Boolean =
        b match {
          case bt: Defn.Trait => a isEqual bt
          case _            => false
        }
    }

  implicit val termBlockStructureEquality =
    new Equality[Term.Block] {
      def areEqual(a: Term.Block, b: Any): Boolean =
        b match {
          case bt: Term.Block => a isEqual bt
          case _              => false
        }
    }
}

trait WordSpecBase extends WordSpec with BaseTest
trait FlatSpecBase extends FlatSpec with BaseTest