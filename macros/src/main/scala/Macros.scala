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
//    variableType(tree) match {
//      case Some(typ) =>
    println(typ.asClass.baseClasses.map(_.asClass.fullName))
    typ.asClass.baseClasses.map(_.asClass).exists(_.fullName.contains("Vista"))
//      case None => false
//    }
  }

  def typesImpl(s: c.Tree): c.Tree = {
    def parseStatement(t: c.universe.Tree): c.universe.Tree = t match {
      case Block(stats, expr) =>
        val sstats = stats.map { s => parseStatement(s)}
        q"{ ..$sstats }"
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

              println(args)
              val arrgs = args.map(t => q"classOf[${variableType(t).name}]")

              q"""
                val $varName = classOf[classes.B].getMethod($simpleMethodName, ..$arrgs)
                if (Vista.isAllowed[classes.B](b, "sayHi", List((ru.typeOf[Int], 5)))) {
                  ${Apply(func, args)}
                }
              """
            } else {
              Apply(func, args)
            }
        }
      case ValDef(mods, variable, tpt, expr) =>
        ValDef(mods, variable, tpt, parseStatement(expr))
      case stat =>
        println(stat)
        stat
    }

    val ret = parseStatement(s)
    println(showCode(ret))
    ret
  }
}

//noinspection UnitMethodIsParameterless
object Macros {

  def typesImpl(c: blackbox.Context)(s: c.Tree): c.Tree = {
    import c.universe._

    def variableType(tree: c.universe.Tree): Option[TypeSymbol] = {
      tree.tpe.typeSymbol match {
        case cs: ClassSymbol => Option(cs.asType)
        case _ => None
      }
    }

    def getType[T: TypeTag](obj: T) = typeOf[T]

    def parseStatement(t: c.universe.Tree): c.universe.Tree = t match {
      case Block(stats, expr) =>
        val sstats = stats.map { s => parseStatement(s)}
        q"{ ..$sstats }"
      //      case ClassDef(mods, tpname, tparams, impl) =>
      //        val ret = impl.body.map { tb => parseStatement(tb) }
      //        ClassDef(mods, tpname, tparams, q"{ ..$ret }")

      //        case q"$mods class $tpname[..$tparams] { $self => ..$stats }" =>
      //          stats.foreach(parseStatement)
      case DefDef(mods, tname, tparams, paramss, tpt, expr) =>
        //        case q"$mods def $tname[..$tparams](...$paramss): $tpt = $expr" =>

        DefDef(mods, tname, tparams, paramss, tpt, parseStatement(expr))
      case q"new ..$parents { ..$body }" =>
        q"new ..$parents { ..$body }"
      case Apply(func, args) =>
        println(func, args)
        if (func.children.nonEmpty) {
          func match {
            case q"$variable.$method[..$tparams]" =>
              println(variable, method, tparams)
          }
          println(func)
          val variable = func.children.head
          val method = func.children(1)
          //            println(variableType(variable), method)
          Apply(func, args)
        } else {
          println(func, args)
          Apply(func, args)
        }

      //        case q"$variable $method[..$tparams](...$tparamss)" =>
      //          variableType(variable) match {
      //            case Some(typ) =>
      //              println(typ.asClass.isSynthetic)
      //              println(typ.asClass.baseClasses)
      //              println(typ.asClass.baseClasses(2).fullName)

      //              println(getType(typ.asClass.baseClasses))

      //              typ.asClass.baseClasses.contains(ts: ClassSymbol => ts == TermName("vistas.Vista"))

      //              println(s"Variable $variable: ${typ.fullName}; method: $method")
      //            case None => println("Error")
      //          }
      case ValDef(mods, variable, tpt, expr) =>
        ValDef(mods, variable, tpt, parseStatement(expr))
      case stat =>
        println(stat)
        stat
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

    //    val statements = s.tree
    //    println(showCode(statements))
    showCode(s)
    parseStatement(s)
    //    s
  }

  def getTypes(s: Any): Any = macro VistaMacros.typesImpl
}