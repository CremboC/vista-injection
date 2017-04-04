package vista.util.meta

import vista.FlatSpecBase
import vista.util.meta.xtensions.XDefn

import scala.meta._

class XDefnTest extends FlatSpecBase {
  behavior of "XDefn"

  it should "correctly detect the signature" in {
    val defn = q"def a(one: Int, two: Double, three: String): Int = ???"
    defn.signature should equal(q"def a(p1: Int, p2: Double, p3: String) = {}")
  }

  it should "detect equal signatures" in {
    val defn  = q"def a(one: Int, two: Double, three: String): Int = ???"
    val defn2 = q"def a(asd: Int, dsa: Double, zxc: String): Int = ???"

    defn.signature should equal(defn2.signature)
  }
}
