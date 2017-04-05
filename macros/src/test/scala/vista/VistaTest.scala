package vista

import java.io.File
import java.nio.file.{Files, Paths}
import java.util.stream.Collectors

import com.twitter.util.Eval
import com.twitter.util.Eval.CompilerException

import scala.collection.JavaConverters._
import scala.meta._

class VistaTest extends FlatSpecBase {
  behavior of "EnableToolsTest"

  it should "execute" in {
    val dummy = getClass.getClassLoader.getResource(".empty")
    val tests = Paths.get(dummy.toURI).getParent

    val sources = Files
      .walk(tests)
      .filter(_.getFileName.toString.matches("""(.+)\.scala$"""))
      .collect(Collectors.toList())
      .asScala
    val expected = Files
      .walk(tests)
      .filter(_.getFileName.toString.matches("""(.+)\.scala_expected$"""))
      .collect(Collectors.toList())
      .asScala

    val all = sources zip expected
    all.ensuring(items =>
      items.forall {
        case (l, r) => r.toString.startsWith(l.toString)
    })

    all.foreach {
      case (source, _) =>
        val parsedS =
          new File(source.toString).parse[Source].get.children.head.asInstanceOf[Defn.Object]
//        val parsedE =
//          new File(expected.toString).parse[Source].get.children.head.asInstanceOf[Defn.Object]

        Vista.expand(parsedS)
    }
  }

  it should "transform and fail to compile ForbidEx1" in {
    val s      = getClass.getClassLoader.getResource("ForbidEx1.scala").toURI
    val parsed = new File(s).parse[Source].get.children.head.asInstanceOf[Defn.Object]

    val result = Vista.expand(parsed)

    assertThrows[CompilerException] {
      new Eval()(result.syntax)
    }
  }

  it should "transform and compile ProductEx" in {
    val s      = getClass.getClassLoader.getResource("ProductEx.scala").toURI
    val parsed = new File(s).parse[Source].get.children.head.asInstanceOf[Defn.Object]

    val result = Vista.expand(parsed)

    val eval: Any = new Eval()(result.syntax)
  }

}