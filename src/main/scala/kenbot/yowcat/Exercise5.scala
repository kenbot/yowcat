package kenbot.yowcat

import Exercise5.{Monoid, MonoidHom}

/**
 * Monoids in code usually look like this; they are a useful 
 * representation of composable data types.
 */
trait UsualMonoid[S] {
  def compose(a: S, b: S): S
  def id: S
}

/**
 * In Category Theory, they look a bit different at first; 
 * a Monoid is a Category with only 1 object. 
 *
 * The monoid's elements are the arrows, and we ignore the object.
 */
object Exercise5 {

  type Monoid[M] = Cat {
    type Obj = Unit
    type Arr = M
  }


  /**
   * Exercise 5a.
   *
   * Implement a monoid as a category, given a set, composition and identity.
   */
  def Monoid[M](set: Stream[M])(composeFn: (M,M) => M, idElement: M): Monoid[M] = new Cat {
    type Obj = Unit
    type Arr = M

    def id(obj: Unit): Arr = idElement
    def comp(f: Arr, g: Arr): Arr = composeFn(f,g)

    def dom(m: M): Unit = ()
    def cod(m: M): Unit = ()

    def objects: Stream[Unit] = Stream(())
    def arrows = set
  }


  type MonoidHom[M,N] = Functor {
    val dom: Monoid[M]
    val cod: Monoid[N]
  }


  /**
   * Exercise 5b.
   *
   * Implement a Monoid Homomorphism between two monoids, given a function 
   * between the underlying sets.
   */
  def MonoidHom[M, N](m: Monoid[M], n: Monoid[N], f: M => N): MonoidHom[M,N] = new Functor {
    val dom: m.type = m
    val cod: n.type = n

    def mapArr(arr: dom.Arr): cod.Arr = f(arr) 
    def mapObj(obj: dom.Obj): cod.Obj = ()
  }
}

/**
 * Exercise 5c.
 *
 * Using the definition we just created, create the following monoids:
 */ 
object Monoids {
  import Sets._

  val intAdd: Monoid[Int] = Monoid(ints)(_ + _, 0)

  val intMult: Monoid[Int] = Monoid(ints)(_ * _, 1)

  val boolAnd: Monoid[Boolean] = Monoid(booleans)(_ && _, true)

  val boolOr: Monoid[Boolean] = Monoid(booleans)(_ || _, false)

  val stringAppend: Monoid[String] = Monoid(strings)(_ + _, "")

  // This is the terminal (or "final") monoid; this means that 
  // every other Monoid has a unique homomorphism to it.
  //
  // As a category, it is the trivial category called "1", and is the terminal
  // category as well: every category has a unique functor to it!
  val unitMonoid: Monoid[Unit] = Monoid(unit)((_,_) => (), ())
}

/**
 * Exercise 5d.
 *
 * Implement the following monoid homomorphisms:
 */
object MonoidHoms {
  import Monoids._

  def anythingToUnit[M](m: Monoid[M]): MonoidHom[M, Unit] = 
    MonoidHom(m, unitMonoid, _ => ()) 


  def stringLength: MonoidHom[String, Int] = 
    MonoidHom(stringAppend, intAdd, _.length)
}


/**
 * Exercise 5e. 
 *
 * A "Free Monoid" generated from a set is the most efficient 
 * way to generate a monoid from some set (which might not be a monoid). 
 *
 * This means that it is created from nothing other than the set itself, 
 * and is itself nothing more than a Monoid.
 *
 * The Free Monoid over a set "A" is just lists of "A"! 
 *
 * Given that Monoids are categories, how does this relate to the Free Category in Exercise 2?
 *
 * Implement the free monoid over a set: 
 */
object FreeMonoid {

  def apply[A](set: Stream[A]): Monoid[List[A]] = 
    Monoid(Sets.lists(set))(_ ++ _, Nil)
}
