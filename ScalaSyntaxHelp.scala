object ScalaSyntaxHelp {
  // Pairs/Tuples
  // Pairs (or higher-arity tuples) are written with parens:
  (1, 2)

  // They can be constructed with an alternative syntax, used
  // to emphasise a pair representing a mapping:
  1 -> 2

  // There are methods to extract the parts:
  (1, 2)._1 // ==> 1
  (1, 2)._2 // ==> 2

  // They can be destructured in a pattern match expression:
  (1,2) match {
    case (a,b) => ??? 
  }

  // Or through a pattern-matching assignment:
  val (a,b) = (1,2) // ==> a = 1, b = 2


  // Lists
  // 
  // The traditional immutable cons list.
  // 
  // Creating a List:
  List(1,2,3,4)

  // Constructing the same list using the cons operator:
  // (It binds to the right, prepending each element on the left)
  1 :: 2 :: 3 :: 4 :: Nil

  // Pattern matching on a list
  List(1,2,3) match {
    case Nil => ???
    case head :: tail => ???
  }

  // Appending two lists
  List(1,2,3) ::: List(4,5,6) 
  // or
  List(1,2,3) ++ List(4,5,6)



  // Streams.
  //
  // We use streams a lot here, to represent sets.
  //
  // They are essentially a lazy (and possibly infinite) cons list, 
  // similar in some ways to lists in Haskell.
  // 
  // Creating a stream:
  Stream(1,2,3,4) 

  // Constructing the same stream using the cons operator:
  // (It binds to the right, prepending each element on the left)
  1 #:: 2 #:: 3 #:: 4 #:: Stream.Empty

  // Pattern matching on a stream:
  Stream(1, 2, 3) match {
    case Stream.Empty => ??? 
    case head #:: tail => ???
  }

  // Appending 2 streams
  Stream(1,2,3) #::: Stream(4,5,6)
  // or 
  Stream(1,2,3) ++ Stream(4,5,6)


  // Combinators
  //
  // Most collections such as as lists and streams (and their shared supertype Seq) 
  // have a similar set of combinators.

  // map
  List(1, 2, 3).map(_ + 1) // ==> List(2,3,4) 

  // flatMap
  List(1, 2, 3).flatMap(n => List(n,n)) // ==> List(1, 1, 2, 2, 3, 3)

  // filter
  List(1, 2, 3).filter(_ > 1) // ==> List(2, 3)

  // collect
  // Collect uses a pattern matching block to simultaneously map and filter:
  List(1, 2, 3).collect {
    case 1 => "One" 
    case 2 => "Two" 
  }
  // ==> List("One", "Two")

  // fold
  List(1, 2, 3).foldLeft(0)(_ + _) // ==> 6 

  // For comprehensions
  //
  // If a class has flatMap and map methods, a for-comprehension can be used, similar to Haskell's do syntax:
  for {
    a <- List(1,2,3)
    b <- List(a*2, a*3, a*4)
    c <- List(b+1, b+2)
  } yield (a, b, c)

  // Which is syntax sugar for:
  List(1,2,3).flatMap { a => 
    List(a*2, a*3, a*4).flatMap { b => 
      List(b+1, b+2).map { c => 
        (a, b, c)
      }
    }
  }

  // Apply methods
  //
  // If an object offers a method called "apply", it can be called with function-like syntax.
  List(1,2,3).apply(2) // ==> 3
  List(1,2,3)(2) // ==> 3


  // Type members
  //
  // Classes can have type members, just like fields or methods. 
  // Type members can be abstract.
  // The type belongs to instances of that class.
  //
  // Any fixed instance 'x' has a singleton type 'x.type'
  trait Outer {
    type Foo
    def foo: Foo
  }

  val c: Outer = ???
  val d: Outer = ???

  // a.Foo is "The type 'Foo', belonging to the fixed instance 'a'"
  val foo1: c.Foo = c.foo
  //val foo2: a.Foo = b.foo // WON'T COMPILE!  b.foo is a b.Foo, not an a.Foo!

  val onlyA: c.type = c
  //val onlyA2: a.type = b // WON'T COMPILE! 'a.type' only allows the single instance 'a'. 

}
