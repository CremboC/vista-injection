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

  def typeOfImpl(s: c.Tree): c.Tree = {
    q"${variableType(s)}"
  }

  def typesImpl(s: c.Tree): c.Tree = {

    val q"$variable.$method(..$args)" = s

    println(variable, method, args)

    val transformer = new Transformer {
      override def transformStats(stats: List[c.universe.Tree], exprOwner: c.universe.Symbol): List[c.universe.Tree] = stats.map {
        case stat@q"$variable.$method(..$args)" =>
//          println(args)
          stat
//        case stat@Apply(func, args) =>
//          func match {
//            case q"$variable.$method[..$tparams]"
//              if baseClassesContainsVista(variable.asInstanceOf[c.universe.Tree]) =>
//
//              val simpleMethodName = Literal(Constant(method.asInstanceOf[TermName].toString))
//              val arrgs = args.map(t => q"classOf[${variableType(t).name}]")
//
//              // hopefully get original class
//              val originalClass = baseClasses(variable.asInstanceOf[c.universe.Tree])(2)
//
//              val tempName = c.freshName()
//
//              q"""
//                  val $tempName = classOf[${originalClass.asType}].getMethod($simpleMethodName, ..$arrgs)
//                  if ($variable.isAllowed($tempName)) $stat else println("Method is forbidden")
//               """
//            case _ => stat
//          }
//        case stat => stat
      }
    }


    val transformed = transformer.transform(s)

    println(showCode(transformed))

    // without this, the transformer makes everything explode
    // http://stackoverflow.com/questions/20936509/scala-macros-what-is-the-difference-between-typed-aka-typechecked-an-untyped
    c.untypecheck(transformed)
  }

  def unionImpl[A : c.WeakTypeTag, B : c.WeakTypeTag](a: c.Expr[A], b: c.Expr[B]) : c.Tree = {
    val unionName = c.freshName("union")

    val unionTerm = TermName(unionName)
    val unionType = TypeName(unionName)

    val leftName = TermName(c.freshName("leftOp"))
    val rightName = TermName(c.freshName("rightOp"))

    val leftType = a.tree.tpe
    val rightType = b.tree.tpe

//    println(leftType)

//    getType(a)
//    println(tag.tpe)

//    val unionTypeSuper = weakTypeOf[C]
//    val unionTypeSuper = typeOf[C]
    val res = q"""
       object $unionTerm {
        implicit def toLeft(ab: $unionType): $leftType = ab.$leftName
        implicit def toRight(ab: $unionType): $rightType = ab.$rightName
       }
       class $unionType(val $leftName : $leftType, val $rightName : $rightType)
       new $unionType($a, $b)
     """


//    println(res)

    c.typecheck(res)
//    res
  }

//  def helloImpl(person: c.Expr[String]): c.Tree = {
//    val q"$p" = person.tree
//    q"""1 + 2"""
//  }
}

object VistaMacros {
  def getTypes(s: Any): Any = macro VistaMacros.typesImpl
  def union[A, B](a: A, b: B): Any = macro VistaMacros.unionImpl[A, B]
  def typeOf(s: Any): Any = macro VistaMacros.typeOfImpl
//  def hello(person: String): Int = macro VistaMacros.helloImpl
}