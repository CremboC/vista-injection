package classes

/**
  * @author paulius
  */
class B {
  def sayHi(a: Int): Unit = {
    println("Hi with param!")
  }

  def sayBye(a: Int): Unit = {
    println("Bye!")
  }

  def sayHi(): Unit = {
    println("Hi without param!")
  }
}
