package kenbot.yowcat

import Exercise5.{Monoid, MonoidHom}

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

  def apply[A](set: Stream[A]): Monoid[List[A]] = ??? 
}
