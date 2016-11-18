import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

class VistaMacros(val c: blackbox.Context) {

  import c.universe._

  def getType[T: TypeTag](obj: T) = typeOf[T]

  def variableType(tree: c.universe.Tree): TypeSymbol = {
    tree.tpe.typeSymbol.asInstanceOf[ClassSymbol].asType
  }

  def baseClassesContainsVista(tree: c.universe.Tree): Boolean = {
    val typ = variableType(tree)
    typ.asClass.baseClasses.map(_.asClass).exists(_.fullName.contains("Vista"))
  }

  def typesImpl(s: c.Tree): c.Tree = {
    val transformer = new Transformer {
      override def transformStats(stats: List[c.universe.Tree], exprOwner: c.universe.Symbol): List[c.universe.Tree] = stats.map {
        case stat@Apply(func, args) =>
          func match {
            case q"$variable.$method[..$tparams]" if baseClassesContainsVista(variable.asInstanceOf[c.universe.Tree]) =>
              val varName = c.freshName(TermName("temp"))
              val simpleMethodName = Literal(Constant(method.asInstanceOf[TermName].toString))
              val arrgs = args.map(t => q"classOf[${variableType(t).name}]")

              q"""
              val $varName = classOf[classes.B].getMethod($simpleMethodName, ..$arrgs)
              if ($variable.isAllowed($varName)) {
                $stat
              } else {
                println("Attempted to run a non-allowed function")
              }
              """
            case _ => stat
          }
        case stat => stat
      }
    }

    println(showCode(s))

    val transformed = transformer.transform(s)

//    println(showCode(transformed))

    //    val ret = s.children.map(stat => parseStatement(stat))
    //    println(showCode(ret))
    //    q"{ ..$ret }"
    //    q"..$collected"
    transformed
  }
}

object Macros {
  def getTypes(s: Any): Any = macro VistaMacros.typesImpl
}