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

    def id(obj: Unit): Arr = ???
    def comp(f: Arr, g: Arr): Arr = ???

    def dom(m: M): Obj = ??? 
    def cod(m: M): Obj = ??? 

    def objects: Stream[Obj] = ???
    def arrows: Stream[Arr] = ???
  }



  type MonoidHom[M,N] = Functor {
    val dom: Monoid[M]
    val cod: Monoid[N]
  }

  /**
   * Exercise 5b.
   *
   * A Monoid Homomorphism is a mapping between monoids, that preserves composition and identity.
   * Since Monoids are categories, a Mon Hom is also a functor.
   *
   * Implement a Monoid Homomorphism between two monoids, given a function 
   * between the underlying sets.
   */
  def MonoidHom[M, N](m: Monoid[M], n: Monoid[N], f: M => N): MonoidHom[M,N] = new Functor {
    val dom: m.type = m
    val cod: n.type = n

    def mapArr(arr: dom.Arr): cod.Arr = ???
    def mapObj(obj: dom.Obj): cod.Obj = ???
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

  val intAdd: Monoid[Int] = ???

  val intMult: Monoid[Int] = ???

  val boolAnd: Monoid[Boolean] = ???

  val boolOr: Monoid[Boolean] = ???

  val stringAppend: Monoid[String] = ???

  // The product of any two monoids M * N is a monoid
  def monoidProduct[M,N](m: Monoid[M], n: Monoid[N]): Monoid[(M,N)] = ??? 

  // This is the terminal (or "final") monoid; this means that 
  // every other Monoid has a unique homomorphism to it.
  //
  // As a category, it is the trivial category called "1", and is the terminal
  // category as well: every category has a unique functor to it!
  val unitMonoid: Monoid[Unit] = ???
}

/**
 * Exercise 5d.
 *
 * Implement the following monoid homomorphisms:
 */
object MonoidHoms {
  import Monoids._

  def anythingToUnit[M](m: Monoid[M]): MonoidHom[M, Unit] = ??? 

  val stringLength: MonoidHom[String, Int] = ??? 

  def composeProduct[M](mon: Monoid[M]): MonoidHom[(M,M), M] = ??? 

  def monoidSquared[M](mon: Monoid[M]): MonoidHom[M, (M,M)] = ??? 
}
