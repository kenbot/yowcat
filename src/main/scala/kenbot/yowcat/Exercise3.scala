package kenbot.yowcat

/**
 * The usual representation of a Category in programming looks something like this. 
 *
 * How does this relate to the concepts in Exercises 1 and 2? 
 *
 * - What are the objects?
 * - What are the arrows?
 * - What are cod and dom?
 */
trait Category[Arr[_,_]] {

  def compose[A,B,C](f: Arr[B,C], g: Arr[A,B]): Arr[A,C] 

  def id[A]: Arr[A,A]
}



/**
 * Exercise 3a.
 *
 * Implement "Hask", the category of types and functions
 */
object Hask extends Category[Function1] {

  def compose[A,B,C](g: B => C, f: A => B): A => C = a => g(f(a))

  def id[A]: A => A = identity
}

import scala.concurrent.Future


/**
 * A Relation is like a function, but more general. 
 *
 * It maps elements in the domain to the codomain; unlike functions, 
 * it might map an element in the domain to zero or multiple elements in the codomain.
 */
trait Rel[Dom, Cod] extends Function[Dom, Stream[Cod]]

object Rel {
  def apply[A,B](f: A => Stream[B]) = new Rel[A,B] {
    def apply(a: A): Stream[B] = f(a)
  }
}

/**
 * Exercise 3b. 
 *
 * Implement a Category for Relations.
 */
object RelationsCategory extends Category[Rel] {

  def compose[A,B,C](g: Rel[B, C], f: Rel[A, B]): Rel[A, C] = 
    Rel(a => f(a).flatMap(g))

  def id[A]: Rel[A, A] = Rel(Stream(_)) 
}


/**
 * Exercise 3c.
 *
 * Notice that in 3b. all we needed was 
 * a) flatMap, and 
 * b) creating a Stream from a value?
 *
 * That means that we have a category for any A => M[B], 
 * where M can be flatMapped and created; that is to say, 
 * M is a monad.
 * 
 * So we can throw away everything we know about Lists or Streams
 * and write a more general version instead!
 * 
 * This is called a "Kleisli category". 
 *
 * Let's implement it.
 */
trait Monad[M[_]] {

  def flatMap[A,B](ma: M[A], f: A => M[B]): M[B]

  def unit[A](a: A): M[A]

  type KleisliFn[A, B] = A => M[B] 

  object KleisliCategory extends Category[KleisliFn] {
    def compose[A,B,C](g: B => M[C], f: A => M[B]): A => M[C] = 
      a => flatMap(f(a), g)

      def id[A]: A => M[A] = unit 
  }
}



