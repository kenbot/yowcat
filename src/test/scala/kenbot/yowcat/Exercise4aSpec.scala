package kenbot.yowcat

import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

object Exercise4aSpec extends Properties("Exercise 4a: Identity Functor") {

  def genCat: Gen[Cat] = new Cat {
    type Obj = Int
    def objects = (1 to 10).toStream

    type Arr = (Int,Int)
    def arrows = {
      def identities = objects.map(n => (n,n))
      identities #::: Stream(1 -> 2, 2 -> 3, 1 -> 3, 7 -> 9)
    }

    def dom(f: Arr) = f._1
    def cod(f: Arr) = f._2

    def id(a: Obj): Arr = (a,a)
    def comp(g: Arr, f: Arr): Arr = (f._1, g._2)
  }

  implicit def arbitraryCat = Arbitrary(genCat)

  property("Identity functor maps to the same arrows") = forAll {
    (cat: Cat) => 
      val f = new IdFunctor(cat)
      f.dom.arrows.map(f.mapArr) == cat.arrows
  }

  property("Identity functor maps to the same objects") = forAll {
    (cat: Cat) => 
      val f = new IdFunctor(cat)
      f.dom.objects.map(f.mapObj) == cat.objects
  }
}
