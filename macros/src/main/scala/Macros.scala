import scala.language.experimental.macros
import scala.reflect.macros.blackbox

class VistaMacros(val c: blackbox.Context) {

  import c.universe._

  def getType[T: TypeTag](obj: T) = typeOf[T]

  def variableType(tree: c.universe.Tree): TypeSymbol = {
    tree.tpe.typeSymbol.asInstanceOf[ClassSymbol].asType
  }

  def baseClasses(tree: c.universe.Tree): Seq[ClassSymbol] = {
    variableType(tree).asClass.baseClasses.map(_.asClass)
  }

  def baseClassesContainsVista(tree: c.universe.Tree): Boolean = {
    baseClasses(tree).exists(_.fullName.contains("Vista"))
  }

  def typesImpl(s: c.Tree): c.Tree = {

    val transformer = new Transformer {
      override def transformStats(stats: List[c.universe.Tree], exprOwner: c.universe.Symbol): List[c.universe.Tree] = stats.map {
        case stat@Apply(func, args) =>
          func match {
            case q"$variable.$method[..$tparams]"
              if baseClassesContainsVista(variable.asInstanceOf[c.universe.Tree]) =>

              val simpleMethodName = Literal(Constant(method.asInstanceOf[TermName].toString))
              val arrgs = args.map(t => q"classOf[${variableType(t).name}]")

              // hopefully get original class
              val originalClass = baseClasses(variable.asInstanceOf[c.universe.Tree])(2)
              val originalClassCons = q"classOf[${originalClass.asType}]"

              q"""
                  if ($variable.isAllowed(classOf[${originalClass.asType}].getMethod($simpleMethodName, ..$arrgs))) $stat else println("Method is forbidden")
               """
            case _ => stat
          }
        case stat => stat
      }
    }


    val transformed = transformer.transform(s)

    // without this, the transformer makes everything explode
    // http://stackoverflow.com/questions/20936509/scala-macros-what-is-the-difference-between-typed-aka-typechecked-an-untyped
    c.untypecheck(transformed)
  }
}

object Macros {
  def getTypes(s: Any): Any = macro VistaMacros.typesImpl
}