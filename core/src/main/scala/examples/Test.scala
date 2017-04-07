package examples

@vista.enable
object Test {
  class Wibble(foo: Int, bar: String)(f: Int) {

    println("Wibble wobble")

    def this(t: (Int, String)) = {
      this(t._1, t._2)(1)
      println("You can execute more code here")
    }

    def this(baz: List[Any]) = {
      this(1, "hi")(2)
      println(this.foo)
      println("You can also execute some code here")
    }

    def g: Int = 1
  }

  def main(args: Array[String]): Unit = {}
}
