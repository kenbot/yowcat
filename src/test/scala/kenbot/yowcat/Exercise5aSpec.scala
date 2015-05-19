package kenbot.yowcat

import org.scalacheck._
import org.scalacheck.Prop._


object Exercise5aSpec extends Properties("Exercise 5a: Monoids as categories") {

  import Exercise5._

  class MonoidTests[M](monoid: Monoid[M]) {
    val catGen = new CatGenerators(monoid)
    implicit val arbObj = catGen.arbObj
    implicit val arbArr = catGen.arbArr

    val untypedUnit: Any = ()

    property("Composing monoid elements must be associative") = 
      catGen.composeAssociative

    property("The id element is neutral composed from the left") = 
      catGen.leftIdentity

    property("The id element is neutral composed from the right") = 
      catGen.rightIdentity

    property("Objects are unit") = 
      Prop(monoid.objects == Sets.unit)

    property("Dom is always unit") = forAll { 
      (f: monoid.Arr) => untypedUnit == monoid.dom(f) 
    }

    property("Cod is always unit") = forAll { 
      (f: monoid.Arr) => untypedUnit == monoid.cod(f)
    }
  }

  new MonoidTests(Monoid(Sets.ints)(_ + _, 0))
  new MonoidTests(Monoid(Sets.booleans)(_ && _, true))
}
