package kenbot.yowcat

import org.scalacheck._
import org.scalacheck.Prop._


object Exercise5eSpec extends Properties("Exercise 5e: Free Monoid") {

  import Exercise5._
  import Monoids._

  import Sets._

  property("Free monoid id is the empty list") =
    Prop(FreeMonoid(booleans).id(()) == Nil)

  property("Free monoid composition appends lists") = forAll {
    (list1: List[Int], list2: List[Int]) => 
    FreeMonoid(ints).compose(list1, list2) == list1 ++ list2
  }

  property("Category laws hold") = {
    new CatGenerators(FreeMonoid(ints)).categoryLaws
  }
}
