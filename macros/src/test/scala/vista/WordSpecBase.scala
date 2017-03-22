package vista

import org.scalatest._

import scala.meta.{Defn, Tree}

/**
  * Created by Crembo on 2017-03-22.
  */

sealed trait BaseTest extends Matchers with ResetsDatabase {
  protected val db = semantics.Database
  protected val addInsts: Tree => Unit = _.traverse {
    case c: Defn.Class => db.add(c)
    case c: Defn.Trait => db.add(c)
  }
}

trait WordSpecBase extends WordSpec with BaseTest
trait FlatSpecBase extends FlatSpec with BaseTest
