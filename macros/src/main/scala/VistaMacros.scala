import scala.language.experimental.macros
import scala.reflect.macros.blackbox

class VistaMacros(val c: blackbox.Context) {

  import c.universe._

  def getType[T: TypeTag](obj: T): c.universe.Type = typeOf[T]

  def variableType(tree: c.universe.Tree): TypeSymbol = {
    tree.tpe.typeSymbol.asInstanceOf[ClassSymbol].asType
  }

  def baseClasses(tree: c.universe.Tree): Seq[ClassSymbol] = {
    variableType(tree).asClass.baseClasses.map(_.asClass)
  }

  def baseClassesContainsVista(tree: c.universe.Tree): Boolean = {
    baseClasses(tree).exists(_.fullName.contains("Vista"))
  }

  def interceptDot(s: c.Tree): c.Tree =  {
    val q"$variable.$method(..$_)" = s
    // only change method calls that are vistas
    if (!baseClassesContainsVista(variable.asInstanceOf[c.universe.Tree])) {
      s
    } else {
      val simpleMethodName = Literal(Constant(method.toString))
      val Apply(_, args) = s
      val methodArgs = args.map(t => q"classOf[${variableType(t).name}]")

      // hopefully get original class
      val originalClass = baseClasses(variable.asInstanceOf[c.universe.Tree])(2)

      val tempName = c.freshName()

      q"""
        val ${TermName(tempName)} = classOf[${originalClass.asType}].getMethod($simpleMethodName, ..$methodArgs)
        if ($variable.isAllowed(${TermName(tempName)})) $s else println("Method is forbidden")
     """
    }
  }
}

object VistaMacros {
  def getTypes(s: Any): Any = macro VistaMacros.interceptDot
}