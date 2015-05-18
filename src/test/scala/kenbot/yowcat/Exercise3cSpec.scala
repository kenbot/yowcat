package kenbot.yowcat

import org.scalacheck._
import org.scalacheck.Prop._


object Exercise3cSpec extends Properties("Exercise 3c: The Kleisli category of a monad") {

  type IntRel = Int => List[Int]

  object ListMonad extends Monad[List] {
    def flatMap[A,B](ma: List[A], f: A => List[B]): List[B] = 
      ma.flatMap(f)
      
    def unit[A](a: A): List[A] = List(a)
  }

  import ListMonad.KleisliCategory

  def genRel(f: (Int,Int) => Int): Gen[IntRel] = { 
    import Gen._, Arbitrary._
    listOf[Int](arbitrary[Int]).map { list => 
      n => list.map(f(_, n))
    }
  }

  implicit def arbitraryRel: Arbitrary[IntRel] = 
    Arbitrary(Gen.oneOf(
      genRel(_ + _), 
      genRel(_ * _), 
      genRel((a,b) => a*a + b*b)))


  property("Composition returns correct value") = forAll {
    (g: IntRel, f: IntRel, n: Int) => 
      KleisliCategory.compose(g,f)(n) == f(n).flatMap(g) 
  }

  property("Composition is associative") = forAll {
    (f: IntRel, g: IntRel, h: IntRel, n: Int) => 
      import KleisliCategory.compose
      compose(h, compose(g,f))(n) == compose(compose(h,g), f)(n)
  }

  property("Identity returns same thing") = forAll {
    (n: Int) => KleisliCategory.id(n) == List(n) 
  }

  property("Identity composes neutrally on the left") = forAll {
    import KleisliCategory._
    (f: IntRel, n: Int) => compose(id[Int], f)(n) == f(n)
  }

  property("Identity composes neutrally on the right") = forAll {
    import KleisliCategory._
    (f: IntRel, n: Int) => compose(f, id[Int])(n) == f(n)
  }
}
