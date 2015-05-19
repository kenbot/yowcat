package kenbot.yowcat

import org.scalacheck._
import org.scalacheck.Prop._


object Exercise3bSpec extends Properties("Exercise 3b: The Category of Relations") {

  type IntRel = Rel[Int,Int]

   def genRel(f: (Int,Int) => Int): Gen[IntRel] = { 
     import Gen._, Arbitrary._
     listOf[Int](arbitrary[Int]).map { list => 
       Rel[Int,Int](n => list.toStream.map(f(_, n)))
     }
   }

  implicit def arbitraryRel: Arbitrary[IntRel] = 
    Arbitrary(Gen.oneOf(
      genRel(_ + _), 
      genRel(_ * _), 
      genRel((a,b) => a*a + b*b)))


  property("Composition returns correct value") = forAll {
    (g: IntRel, f: IntRel, n: Int) => 
      RelationsCategory.compose(g,f)(n) == f(n).flatMap(g) 
  }

  property("Composition is associative") = forAll {
    (f: IntRel, g: IntRel, h: IntRel, n: Int) => 
      import RelationsCategory.compose
      compose(h, compose(g,f))(n) == compose(compose(h,g), f)(n)
  }

  property("Identity returns same thing") = forAll {
    (n: Int) => RelationsCategory.id(n) == Stream(n) 
  }

  property("Identity composes neutrally on the left") = forAll {
    import RelationsCategory._
    (f: IntRel, n: Int) => compose(id[Int], f)(n) == f(n)
  }

  property("Identity composes neutrally on the right") = forAll {
    import RelationsCategory._
    (f: IntRel, n: Int) => compose(f, id[Int])(n) == f(n)
  }
}
