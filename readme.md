## Vista Injection using Scala meta-programming

### Issues
#### Traitification
Classes with the same constructor value name
```scala
  trait A {
    val a: String
  }

  trait B {
    val a: String
  }

  val a = new A with B {
    override val a: String = "hello"
  }
```

#### Class constructor to trait conversion
* Cannot use `final` since members must be final
```scala
trait A {
  private final val a: String
}
// 'final' modifier can't be used with incomplete members
```
* Cannot use `private` since abstract members cannot be private
```scala
trait A {
  private val a: String
}
// abstract member may not have private modifier
```

This is probably _good_ since when we instantiate traits after running 
an op we want it to be accessible.
#### Ops
```scala
  class A(p: Int)
  class B
  
  val a = new A(1)
  val b = new B
  
  val ab: AuB = ∩[A, B](a, b)
  // object creation is impossible since value p in Trait A of type Int is not defined
  // val ab = new AuB with vistas.AnyV {}
```

Should actually create
```scala
  trait A {
    val p: Int
  }
  trait B
  
  val a = new A {
    override val p = 1
  }
  val b = new B {}
  trait AuB extends A with B
  val ab = new AuB {
    override val p = a.p
  }
```

Problem is what to do with a.p is private? Technically, it is impossible with traits since they
cannot have private members.
**Solved by 289ef5c**

#### Constructor parameter types
Call-by-name will not work since
```scala
  class A(f: => Unit)
```
needs to be rewritten as
```scala
  trait A {
    val f: () => Unit
  }
```
which is no longer call-by-name and actually is just a normal function with 
no parameters and returning unit.