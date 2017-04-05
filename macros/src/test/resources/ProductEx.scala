object ProductEx {
  class Ap {
    def zero: Int = 0
  }

  class A extends Ap {
    def one(): Int = 1
    def two(): Int = 2
  }

  class B(val s: String) {
    def two(): Int   = 2
    def three(): Int = 3
  }

  def main(args: Array[String]): Unit = {
    val a = new A
    val b = new B

    val ab = ⨯[A, B, AxB](a, b)

    ab.onetwo()()
  }
}
