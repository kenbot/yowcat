package kenbot.yowcat

import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

object Exercise4cSpec extends Properties("Exercise 4c: Category of categories & functors") {

  val catGen: CatGenerators[CatCat.type] = 
    new CatGenerators(CatCat)

  import catGen._

  property("Functors compose associatively") = composeAssociative

  property("Composed functors preserve the first category's domain") = composeDomain

  property("Composed functors preserve the second category's codomain") = composeCodomain

  property("The set of arrows contains all compositions") = arrowsContainCompositions

  property("The identity functor has the same object as domain") = identityDomain
  
  property("The identity functor has the same object as codomain") = identityCodomain

  property("The identity functor composes neutrally from the left") = leftIdentity

  property("The identity functor composes neutrally from the right") = rightIdentity

  property("The set of arrows contains all identities") = arrowsContainIdentities
}
