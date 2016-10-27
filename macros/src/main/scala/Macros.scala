import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

//noinspection UnitMethodIsParameterless
object Macros {

  def interceptNewImpl(c: blackbox.Context)(s: c.Tree): c.Expr[Any] = {
    import c.universe._

    val ret = s.children.map {
      case ValDef(mods, name, tyt, rhs) =>
        println(name, tyt, rhs)
        ValDef(mods, name, tyt, rhs)
      case New(s) =>
        New(s)
      case Literal(s) =>
        Literal(s)
      case s =>
        s
    }

    println(ret)

//    val a = reify(ret:_*)

//    c match {
//      case q"$left = $right" => println(left, right)
//    }
//    s

//    val a = ret.reduceLeft[c.Tree] {
//      case (t, stat) => q"$t $stat"
//    }
//    println(a)
//    ret.reduce[c.Tree] {
//      case (t, s) =>
//    }


//    Block(ret.head)
//    ValDef(Modifiers(), TermName("a"), TypeN)
    q"""
       { ..$ret }
      """
    reify {

    }
    c.Expr[Any](ret.head)
  }

  def interceptNew(s: Any): Any = macro interceptNewImpl

  def typesImpl(c: blackbox.Context)(s: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._

    def variableType(tree: c.universe.Tree): TypeSymbol = {
      tree.tpe.typeSymbol.asInstanceOf[ClassSymbol].asType
    }
    def getType[T: TypeTag](obj: T) = typeOf[T]

    def parseStatement(t: c.universe.Tree): Unit = {
      t.foreach {
        case q"$mods class $tpname[..$tparams] { $self => ..$stats }" =>
          stats.foreach(parseStatement)
        case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>
          parseStatement(expr)
        case q"$variable $method[..$tparams](...$tparamss)" =>
          println(s"Variable $variable: ${variableType(variable).fullName}; method: $method")
        case q"$mods val $variable: $tpt = $expr" =>
        case stat =>
      }
    }

//    val result = s.tree.children.map {
//      case q"$variable $method[..$tparams](...$tparamss)" =>
////        println(s"Variable $variable: ${variableType(variable).fullName}; method: $method")
//        q"$variable $method[..$tparams](...$tparamss)"
//      case q"$mods val $variable: $tpt = $expr" =>
////        println(expr)
//
//        q"$mods val $variable: $tpt = $expr"
//      case stats =>
//        q"$stats"
//    }.asInstanceOf[List[c.universe.Tree]]
//
//    s.tree.children.foreach {
//      case Apply(a, b) =>
//        println(a, b)
//      case Select(a, b) =>
//        println(a, b)
//      case _ =>
//    }


//    println(result)

    val statements = s.tree
    parseStatement(statements)
    s
  }

  def getTypes(s: Any): Any = macro typesImpl
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