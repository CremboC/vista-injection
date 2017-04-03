package vista.util.meta

import scala.collection.immutable.Seq
import scala.meta._

/**
  * Created by Crembo on 2017-03-21.
  */
trait XDecl {
  implicit class XDeclVal(self: Decl.Val) {
    def asDefnVal: Defn.Val = {
      val q"..$mods val ..$pnamesnel: $tpe" = self
      Defn.Val(mods, pnamesnel, Option(tpe), rhs = Term.Block(Seq.empty))
    }
  }

  implicit class XDeclVar(self: Decl.Var) {
    def asDefnVar: Defn.Var = {
      val q"..$mods var ..$pnamesnel: $tpe" = self
      Defn.Var(mods, pnamesnel, Option(tpe), None)
    }
  }
}
