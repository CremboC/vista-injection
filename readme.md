## Vista Injection using Scala meta-programming

### Edge-cases
#### Traitification
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