package vista.internal

import java.io.File
import java.nio.file.{Files, Paths}
import java.util.stream.Collectors

import com.twitter.util.Eval
import com.twitter.util.Eval.CompilerException
import vista.FlatSpecBase

import scala.collection.JavaConverters._
import scala.meta._
import scala.meta.contrib._

class EnableTest extends FlatSpecBase {
  behavior of "Enabler"

  it should "parse correctly" in {
    val dummy = getClass.getClassLoader.getResource(".empty")
    val tests = Paths.get(dummy.toURI).getParent

    val sources = Files
      .walk(tests)
      .filter(_.getFileName.toString.matches("""(.+)\.scala$"""))
      .collect(Collectors.toList())
      .asScala

    sources foreach { s =>
      val parsed = new File(s.toString).parse[Source].get.extract[Defn.Object].head
      Enable(parsed)
    }
  }

  it should "transform and fail to compile ForbidEx1" in {
    val s      = getClass.getClassLoader.getResource("ForbidEx1.scala").toURI
    val parsed = new File(s).parse[Source].get.children.head.asInstanceOf[Defn.Object]

    val result = Enable(parsed)

    assertThrows[CompilerException] {
      new Eval()(result.syntax)
    }
  }

  it should "transform and compile ProductEx" in {
    val s      = getClass.getClassLoader.getResource("ProductEx.scala").toURI
    val parsed = new File(s).parse[Source].get.children.head.asInstanceOf[Defn.Object]

    val result = Enable(parsed)

    val eval: Any = new Eval()(result.syntax)
  }

}
