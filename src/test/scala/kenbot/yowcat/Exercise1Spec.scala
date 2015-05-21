package kenbot.yowcat

import org.scalacheck._
import org.scalacheck.Prop._


object Exercise1Spec extends Properties("Exercise 1: Category of Classes and subtypes") {
  import SubTypeSugar._

  val catGen = new CatGenerators(Exercise1.ClassesSubtypesCategory)

  property("Composing subtype relationships must be associative") = 
    catGen.composeAssociative

  property("Classes are their own subtype") = 
    catGen.leftIdentity 

  property("Classes are their own subtype (right)") = 
    catGen.rightIdentity

  property("Composed subtype relationships preserve the top class") = 
    catGen.composeDomain
    
  property("Composed subtype relationships preserve the bottom class") = 
    catGen.composeCodomain

  property("The identity subtype relationship should have the domain of that Class") = 
    catGen.identityDomain
  
  property("The identity subtype relationship should have the codomain of that Class") = 
    catGen.identityCodomain

  property("all arrows should be subtypes") =
    Exercise1.ClassesSubtypesCategory.arrows.forall { case (c, d) => d.isSubTypeOf(c) }
}
