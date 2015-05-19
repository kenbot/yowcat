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

  property("Monoid product has the correct parameters") = forAll { 
    val intAdd = Monoid(Sets.ints)(_ + _, 0)
    val stringAppend = Monoid(Sets.strings)(_ + _, "")
    val prod: Monoid[(Int, String)] = monoidProduct(intAdd, stringAppend)
    (i1: Int, i2: Int, s1: String, s2: String) => 

      val compOk = prod.compose((i1, s1), (i2, s2)) == (i1 + i2, s1 + s2)
      val idOk = prod.id(()) == (0, "")
      Prop(idOk && compOk) 
  }

}
