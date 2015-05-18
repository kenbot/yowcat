package kenbot.yowcat

import org.scalacheck._
import org.scalacheck.Prop._


object Exercise2Spec extends Properties("Exercise 2: Free Categories from graphs") {


  def genGraph: Gen[Graph] = {
    def chooseNode = Gen.choose(0, 10)

    def genEdge: Gen[(Int,Int)] = for {
      a <- chooseNode
      b <- chooseNode
    } yield (a,b)

    
    Gen.listOfN(10, genEdge).map { edgeList => 
      new Graph {
        type Node = Int
        def edges = Stream(1->2, 2->3, 3->4, 4->5, 5->6) #::: edgeList.toStream
      }
    }
  }

  implicit def arbitraryFreeCategory: Arbitrary[FreeCategory] = 
    Arbitrary(genGraph.map(g => new FreeCategory(g)))


  property("Composing graph paths must be associative") = forAll {
    (freeCat: FreeCategory) => 
    new CatGenerators(freeCat).composeAssociative
  }

  property("Paths should contain identities") = forAll {
    (freeCat: FreeCategory) => 
    new CatGenerators(freeCat).arrowsContainIdentities
  }

  property("Empty path can compose on the left") = forAll {
    (freeCat: FreeCategory) => 
    new CatGenerators(freeCat).leftIdentity
  }

  property("Empty path can compose on the right") = forAll {
    (freeCat: FreeCategory) => 
    new CatGenerators(freeCat).rightIdentity
  }

  property("Composed paths preserve the start node") = forAll {
    (freeCat: FreeCategory) => 
    new CatGenerators(freeCat).composeDomain
  }
    
  property("Composed paths preserve the end node") = forAll {
    (freeCat: FreeCategory) => 
    new CatGenerators(freeCat).composeCodomain
  }


  property("Empty path starts on the node") = forAll {
    (freeCat: FreeCategory) => 
    new CatGenerators(freeCat).identityDomain
  }
  
  property("Empty path ends on the node") = forAll {
    (freeCat: FreeCategory) => 
    new CatGenerators(freeCat).identityCodomain
  }
}
