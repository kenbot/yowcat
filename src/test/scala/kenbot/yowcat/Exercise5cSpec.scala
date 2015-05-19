package kenbot.yowcat

import org.scalacheck._
import org.scalacheck.Prop._


object Exercise5cSpec extends Properties("Exercise 5c: Specific monoids") {

  import Exercise5._

  import Monoids._

  property("Integer addition monoid has the correct parameters") = forAll { 
    (a: Int, b: Int) => 
      (intAdd.compose(a,b) == (a + b)) && 
      (intAdd.id(()) == 0)
  }

  property("Integer multiplication monoid has the correct parameters") = forAll { 
    (a: Int, b: Int) => 
      (intMult.compose(a,b) == (a * b)) && 
      (intMult.id(()) == 1)
  }

  property("String append monoid has the correct parameters") = forAll { 
    (a: String, b: String) => 
      (stringAppend.compose(a,b) == (a + b)) && 
      (stringAppend.id(()) == "")
  }

  property("Boolean AND monoid has the correct parameters") = forAll { 
    (a: Boolean, b: Boolean) => 
      (boolAnd.compose(a,b) == (a && b)) && 
      (boolAnd.id(()) == true)
  }

  property("Boolean OR monoid has the correct parameters") = forAll { 
    (a: Boolean, b: Boolean) => 
      (boolOr.compose(a,b) == (a || b)) && 
      (boolOr.id(()) == false)
  }

  property("Unit monoid has the correct parameters") = { 
    val nonWhinyUnit: Any = ()
    Prop((unitMonoid.compose((),()) == nonWhinyUnit) && 
         (unitMonoid.id(()) == nonWhinyUnit))
  }

}
