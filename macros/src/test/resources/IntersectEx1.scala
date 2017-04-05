object IntersectEx1 {
  class Ap {
    def zero: Int = -0
  }

  class A extends Ap {
    def one(): Int    = 1
    def two(): Double = 2.10
  }

  class B {
    def zero: Int     = 0
    def two(): Double = 2.11
    def three(): Int  = 3
  }

  class C(val a: String)

  def main(args: Array[String]): Unit = {
    val a = new A
    val b = new B

    val c = new C("test")

    val ab = ∩[A, B, AuB](a, b)

    if ({ def zero: Int = ??? }.⊆[AuB](ab)) {
      ab.zero
    } else {
      println("ab.zero is not allowed")
    }

    if ({ def one: Int = ??? }.⊆[AuB](ab)) {
      ab.one()
    } else {
      println("ab.one is not allowed")
    }

    ab.one()   // compiler error
    ab.two()   // valid
    ab.three() // compiler error
  }
}
