package kenbot.yowcat

import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

object Exercise4bSpec extends Properties("Exercise 4b: Functor between finite categories") {

  val catGen1: CatGenerators[Cat1.type] = 
    new CatGenerators(Cat1)

  import catGen1._


  property("Mapping from Cat1 -> Cat2 preserves identity") = forAll {
    import Cat1Cat2Functor._
    (a: Cat1.Obj) => 
      mapArr(Cat1.id(a)) == Cat2.id(mapObj(a)) 
  }

  property("Mapping from Cat1 -> Cat2 preserves composition") = forAll {
    import Cat1Cat2Functor._
    import catGen1.Composable2
    (cmp: Composable2) => 
      val Composable2(g,f) = cmp

      mapArr(Cat1.compose(g,f)) == Cat2.compose(mapArr(g), mapArr(f)) 
  }
}
