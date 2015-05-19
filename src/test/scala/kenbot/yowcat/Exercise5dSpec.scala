package kenbot.yowcat

import org.scalacheck._
import org.scalacheck.Prop._


object Exercise5dSpec extends Properties("Exercise 5d: Specific monoid homomorphisms") {

  import Exercise5._
  import MonoidHoms._

  val intAdd = Monoid(Sets.ints)(_ + _, 0)
  val stringAppend = Monoid(Sets.strings)(_ + _, "")
  val nonWhinyUnit: Any = ()

  property("Anything -> Unit preserves identity") = 
    Prop(anythingToUnit(intAdd).mapArr(intAdd.id(())) == nonWhinyUnit)

  property("Anything -> Unit preserves composition") = forAll {
    (a: Int, b: Int) => 
      anythingToUnit(intAdd).mapArr(intAdd.compose(a,b)) == nonWhinyUnit
  }

  property("String length preserves identity") = 
    Prop(stringLength.mapArr(stringAppend.id(())) == intAdd.id(()))

  property("String length preserves composition") = forAll {
    import stringLength._
    (a: String, b: String) => 
      Prop(mapArr(stringAppend.compose(a,b)) == 
           intAdd.compose(mapArr(a), mapArr(b)))
  }

  //property("Composing M*M preserves identity") = { 
  //  Prop(stringLength.mapArr(stringAppend.id(())) == intAdd.id(()))

}
