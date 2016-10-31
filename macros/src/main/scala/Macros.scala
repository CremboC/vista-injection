import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

//noinspection UnitMethodIsParameterless
object Macros {

  def interceptNewImpl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    def variableType(tree: c.universe.Tree): Option[TypeSymbol] = {
      tree.tpe.typeSymbol match {
        case cs: ClassSymbol => Option(cs.asType)
        case _ => None
      }
    }

    def parseTree(tree: c.Tree): c.Tree = tree match {
      case ValDef(mods, name, tyt, rhs) =>
        println(showCode(rhs))
        val regex = """new ([A-Za-z]+)\(\)""".r
        val regex(clazz) = showCode(rhs)
        ValDef(mods, name, tyt, q"new $clazz with vistas.Vista[$clazz]")
      case DefDef(mods, tname, tparams, paramss, tpt, expr) =>
        val parsed = expr.children.map(ex => parseTree(ex))
        q"$mods def $tname[..$tparams](...$paramss): $tpt = {..$parsed}"
      case q"$variable $method[..$tparams](...$tparamss)" =>
        println(variable, method, tparamss)
        q"$variable $method[..$tparams](...$tparamss)"
      case q"{..$stats}" =>
        q"{..$stats}"
    }

    val ret = parseTree(annottees.head)

    println(showCode(ret))

    q"""{ ..$ret }"""
  }



  def typesImpl(c: blackbox.Context)(s: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._

    def variableType(tree: c.universe.Tree): Option[TypeSymbol] = {
      tree.tpe.typeSymbol match {
        case cs: ClassSymbol => Option(cs.asType)
        case _ => None
      }
    }
    def getType[T: TypeTag](obj: T) = typeOf[T]

    def parseStatement(t: c.universe.Tree): Unit = {
      t.foreach {
        case q"$mods class $tpname[..$tparams] { $self => ..$stats }" =>
          stats.foreach(parseStatement)
        case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>
          parseStatement(expr)
        case q"new ..$parents { ..$body }" =>
          println(parents)
        case q"$variable $method[..$tparams](...$tparamss)" =>
          variableType(variable) match {
            case Some(typ) =>
              println(typ.asClass.isSynthetic)
              println(typ.asClass.baseClasses(2).fullName)
              println(s"Variable $variable: ${typ.fullName}; method: $method")
            case None => println("Error")
          }
        case q"$mods val $variable: $tpt = $expr" =>
          parseStatement(expr)
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
//    println(showCode(statements))
    parseStatement(statements)
    s
  }

  def getTypes(s: Any): Any = macro typesImpl
}

@compileTimeOnly("enable macro paradise to expand macro annotations")
class noop extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro linkMacro.impl
}

@compileTimeOnly("enable macro paradise to expand macro annotations")
class interceptNew extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Macros.interceptNewImpl
}

//def interceptNew[A](s: Any): A = macro interceptNewImpl

object linkMacro {
  def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    c.Expr[Any](q"{..$annottees}")
  }
}