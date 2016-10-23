import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.blackbox

//noinspection UnitMethodIsParameterless
object Macros {


  def impl(c: blackbox.Context)(s: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._

    def variableType(tree: Trees#Tree): TypeSymbol = {
      tree.tpe.typeSymbol.asInstanceOf[ClassSymbol].asType
    }
    def getType[T: TypeTag](obj: T) = typeOf[T]

    def parseStatement(t: Trees#Tree): Unit = {
      t.foreach {
        case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
          stats.foreach(parseStatement)
        case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>
          parseStatement(expr)
        case q"$variable $method[..$tparams](...$tparamss)" =>
          println(s"Variable $variable: ${variableType(variable).fullName}; method: $method")
        case q"$mods val $variable: $tpt = $expr" =>
        case stat =>
      }
    }

    val statements = s.tree
    parseStatement(statements)


    reify {
      println(s"getTypes ${s.splice}!")
    }
  }

  def getTypes(s: Any): Unit = macro impl
}

@compileTimeOnly("enable macro paradise to expand macro annotations")
class noop extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro linkMacro.impl
}

object linkMacro {
  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    c.Expr[Any](q"{..$annottees}")
  }
}