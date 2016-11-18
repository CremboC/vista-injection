package classes


class A

/**
  * @author paulius
  */
class B extends A {


  def sayHi(a: Int): Unit = {
    println("Hi with param!")
  }

  def say(word: String): Unit = {
    println(word)
  }

  def multi(a: Int)(b: Int): Int = a + b

  def sayBye[A](a: Int): Unit = {
    println("Bye!")
  }

  def sayHi(): Unit = {
    println("Hi without param!")
  }
}
