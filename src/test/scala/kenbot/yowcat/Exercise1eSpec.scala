package kenbot.yowcat

import org.scalacheck._
import org.scalacheck.Prop._


object Exercise1eSpec extends Properties("Exercise 1e: Category of Posets") {

  import Exercise1._
  
  implicit def arbitraryPoset: Arbitrary[PosetCategory[_]] = 
    Arbitrary(Generators.fromStream(Sets.posets))

  property("Ordering relationships are associative") = forAll { 
    (poset: PosetCategory[_]) => 
      new CatGenerators(poset).composeAssociative
  }

  property("Composed subtype relationships preserve the top class") = forAll { 
    (poset: PosetCategory[_]) => 
      new CatGenerators(poset).composeDomain
  }
    
  property("Composed subtype relationships preserve the bottom class") = forAll { 
    (poset: PosetCategory[_]) => 
      new CatGenerators(poset).composeCodomain
  }

  property("An object <= itself") = forAll { 
    (poset: PosetCategory[_]) => 
      val catGen = new CatGenerators(poset)
      catGen.leftIdentity && catGen.rightIdentity
  }

  property("Identity of A == 'A <= A'") = forAll { 
    (poset: PosetCategory[_]) => 
      val catGen = new CatGenerators(poset)
      catGen.identityDomain && catGen.identityCodomain
  }

  property("All arrows should be correctly ordered") = forAll {
    (poset: PosetCategory[_]) =>
      import poset._
      arrows.forall { arr =>
        order(cod(arr), dom(arr))
      }
  }
}
