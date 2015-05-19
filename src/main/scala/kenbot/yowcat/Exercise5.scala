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

  // Some streams to use as sets, both finite & infinite
  import Sets.{ints, booleans, strings, unit}

  val intAdd: Monoid[Int] = Monoid(ints)(_ + _, 0)

  val intMult: Monoid[Int] = Monoid(ints)(_ * _, 1)

  val boolAnd: Monoid[Boolean] = Monoid(booleans)(_ && _, true)

  val boolOr: Monoid[Boolean] = Monoid(booleans)(_ || _, false)

  val stringAppend: Monoid[String] = Monoid(strings)(_ + _, "")

  // The product of any two monoids M * N is a monoid
  def pairMonoid[M,N](m: Monoid[M], n: Monoid[N]): Monoid[(M,N)] = Monoid(m.arrows zip n.arrows)(
    (mn1, mn2) => (m.compose(mn1._1, mn2._1), 
                   n.compose(mn1._2, mn2._2)), (m.id(()), n.id(())))

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


  val stringLength: MonoidHom[String, Int] = 
    MonoidHom(stringAppend, intAdd, _.length)

  def composeProduct[M](m: Monoid[M]): MonoidHom[(M,M), M] = ???

  def monoidSquared[M](m: Monoid[M]): MonoidHom[M, (M,M)] = ???
}
