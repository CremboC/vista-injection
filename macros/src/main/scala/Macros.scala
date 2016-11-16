import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.blackbox


@compileTimeOnly("enable macro paradise to expand macro annotations")
class interceptNew extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro VistaMacros.interceptNew
}

class VistaMacros(val c: blackbox.Context) {

  import c.universe._

  def getType[T: TypeTag](obj: T) = typeOf[T]

  def variableType(tree: c.universe.Tree): TypeSymbol = {
    tree.tpe.typeSymbol.asInstanceOf[ClassSymbol].asType
  }

  def interceptNew(annottees: c.Tree*): c.Tree = {
    val newClassRegex = """new ([A-Za-z]+)\(\)""".r

    def parseTree(tree: c.Tree): c.Tree = tree match {
      case ValDef(mods, name, tyt, rhs) =>
        val newClassRegex(clazz) = showCode(rhs)
        ValDef(mods, name, tyt, q"new ${TypeName(clazz)} with vistas.Vista")
      case DefDef(mods, tname, tparams, paramss, tpt, expr) =>
        val parsed = expr.children.map(ex => parseTree(ex))
        q"$mods def $tname[..$tparams](...$paramss): $tpt = {..$parsed}"
      case Apply(func, args) =>
        Apply(func, args)
      case q"$variable $method[..$tparams](...$tparamss)" =>
        q"$variable $method[..$tparams](...$tparamss)"
      case q"{ case ..$cases }" =>
        q"{ case ..$cases }"
      case Block(stats, expr) =>
        val ss = stats.map { s => parseTree(s) }
        q"{..$ss}"
    }

    val ret = parseTree(annottees.head)

//    println(showCode(ret))

    q"""{ ..$ret }"""
  }

  def baseClassesContainsVista(tree: c.universe.Tree): Boolean = {
    val typ = variableType(tree)
    typ.asClass.baseClasses.map(_.asClass).exists(_.fullName.contains("Vista"))
  }

  def typesImpl(s: c.Tree): c.Tree = {
    def parseStatement(t: c.universe.Tree): c.universe.Tree = t match {
      case Block(stats, expr) =>
        val sstats = stats.map { s => parseStatement(s)}
//        println(stats, expr)
        q"{ ..${sstats :+ parseStatement(expr) } }"
      //      case ClassDef(mods, tpname, tparams, impl) =>
      //        val ret = impl.body.map { tb => parseStatement(tb) }
      //        ClassDef(mods, tpname, tparams, q"{ ..$ret }")

      //        case q"$mods class $tpname[..$tparams] { $self => ..$stats }" =>
      //          stats.foreach(parseStatement)
      case DefDef(mods, tname, tparams, paramss, tpt, expr) =>
        DefDef(mods, tname, tparams, paramss, tpt, parseStatement(expr))
      case q"new ..$parents { ..$body }" =>
        q"new ..$parents { ..$body }"
      case Apply(func, args) =>
        func match {
          case q"$variable.$method[..$tparams]" =>
            if (baseClassesContainsVista(variable.asInstanceOf[c.universe.Tree])) {
              val varName = c.freshName(TermName("temp"))
              val simpleMethodName = Literal(Constant(method.asInstanceOf[TermName].toString))
              val arrgs = args.map(t => q"classOf[${variableType(t).name}]")

              q"""
                val $varName = classOf[classes.B].getMethod($simpleMethodName, ..$arrgs)
                if ($variable.isAllowed($varName)) {
                  ${Apply(func, args)}
                } else {
                  println("Attempted to run a non-allowed function")
                }
              """
            } else {
              Apply(func, args)
            }
          case _ =>
            println("Not matched")

//            c.untypecheck()
            Apply(func, args.map(arg => c.untypecheck(arg)))
        }
//        Apply(func, args)
      case TypeApply(func, args) =>
        println(func, args)
        TypeApply(func, args)
//      case q"$varr $method[..$tparams](...$args)" =>
////        println(variable, method, tparams, args)
//
//        val variable = varr.asInstanceOf[c.universe.Tree]
//        if (baseClassesContainsVista(variable.asInstanceOf[c.universe.Tree])) {
//          val argsAsTree = args.head.asInstanceOf[List[c.universe.Tree]]
//
//          println(argsAsTree)
//
//          val varName = c.freshName(TermName("temp"))
//          val simpleMethodName = Literal(Constant(method.asInstanceOf[TermName].toString))
//          val arrgs = argsAsTree.map(t => q"classOf[${variableType(t).name}]")
//
//          val funcCall = q"$variable.$method[..$tparams](..${args.head})"
//          q"""
//                val $varName = classOf[classes.B].getMethod($simpleMethodName, ..$arrgs)
//                if ($variable.isAllowed($varName)) {
//                  $funcCall
//                } else {
//                  println("Attempted to run a non-allowed function")
//                }
//              """
//        } else {
//          q"$varr $method[..$tparams](...${args.head})"
////          if (args.nonEmpty) q"$varr.$method[..$tparams](..${args.head})"
////          else q"$varr.$method[..$tparams]()"
//        }


//      case Apply(func, args) =>
//        func match {
//          case q"$variable.$method[..$tparams]" =>
//
//        }
      case ValDef(mods, variable, tpt, expr) =>
        ValDef(mods, variable, tpt, parseStatement(expr))
      case tt: c.universe.Tree => tt
    }

    val ret = parseStatement(s)
    println(showCode(ret))
    ret
  }
}

object Macros {
  def getTypes(s: Any): Any = macro VistaMacros.typesImpl
}