package kenbot.yowcat

import org.scalacheck._
import org.scalacheck.Prop._


object Exercise3aSpec extends Properties("Exercise 3a: Categories in Hask") {

  type IntFn = Int => Int

   def genIntFn(f: (Int,Int) => Int): Gen[IntFn] = { 
     import Arbitrary._
     arbitrary[Int].map(a => b => f(a,b)) 
   }

  implicit def arbitraryFn: Arbitrary[IntFn] = 
    Arbitrary(Gen.oneOf(
      genIntFn(_ + _), 
      genIntFn(_ * _), 
      genIntFn((a,b) => a*a + b*b)))

  property("Composition returns correct value") = forAll {
    (f: IntFn, g: IntFn, n: Int) => 
      Hask.compose(g,f)(n) == g(f(n)) 
  }

  property("Composition is associative") = forAll {
    (f: IntFn, g: IntFn, h: IntFn, n: Int) => 
      import Hask.compose
      compose(h, compose(g,f))(n) == compose(compose(h,g), f)(n)
  }

  property("Identity returns same thing") = forAll {
    (n: Int) => Hask.id(n) == n 
  }

  property("Identity composes neutrally on the left") = forAll {
    import Hask._
    (f: IntFn, n: Int) => compose(id[Int], f)(n) == f(n)
  }

  property("Identity composes neutrally on the right") = forAll {
    import Hask._
    (f: IntFn, n: Int) => compose(f, id[Int])(n) == f(n)
  }
}
