package kenbot.yowcat

import org.scalacheck._
import org.scalacheck.Prop._


class CatGenerators(val cat: Cat) {
  import cat._

  private case class Composable2(g: Arr, f: Arr)
  private val composable2: Stream[Composable2] = for {
    f <- arrows
    g <- arrows
    if canCompose(g,f)
  } yield Composable2(g, f)

  private case class Composable3(f: Arr, g: Arr, h: Arr)
  private val composable3: Stream[Composable3] = for {
    Composable2(g, f) <- composable2
    h <- arrows
    if canCompose(h,g)
  } yield Composable3(f, g, h)

  private implicit def arbComposable2: Arbitrary[Composable2] = 
    Arbitrary(Generators.fromStream(composable2))

  private implicit def arbComposable3: Arbitrary[Composable3] = 
    Arbitrary(Generators.fromStream(composable3))

  private implicit def arbObj: Arbitrary[Obj] = 
    Arbitrary(Generators.fromStream(objects))

  private implicit def arbArr: Arbitrary[Arr] = 
    Arbitrary(Generators.fromStream(arrows))


   //    f      g      h
   // A ---> B ---> C ---> D 
  def composeAssociative: Prop = forAll {
    (arrs: Composable3) =>
      val Composable3(f,g,h) = arrs
      compose(h, compose(g,f)) == compose(compose(h,g), f)
  }
  
  //    f      g 
  // A ---> B ---> C
  def composeDomain: Prop = forAll {
    (arrs: Composable2) => 
      val Composable2(g,f) = arrs
      dom(compose(g, f)) == dom(f)
  }

  //    f      g 
  // A ---> B ---> C
  def composeCodomain: Prop = forAll {
    (arrs: Composable2) => 
      val Composable2(g,f) = arrs
      cod(compose(g, f)) == cod(g)
  }

  def arrowsContainIdentities: Prop = forAll {
    (o: Obj) => arrows.contains(id(o))
  }

  def arrowsContainCompositions: Prop = forAll {
    (arrs: Composable2) => 
      val Composable2(g,f) = arrs
      arrows.contains(compose(g,f)) 
  }

  def identityDomain: Prop = forAll {
    (o: Obj) => dom(id(o)) == o
  }
  
  def identityCodomain: Prop = forAll {
    (o: Obj) => cod(id(o)) == o
  }

  def leftIdentity: Prop = forAll {
    (f: Arr) => 
      compose(id(cod(f)), f) == f 
  }

  def rightIdentity: Prop = forAll {
    (f: Arr) => 
      compose(f, id(dom(f))) == f 
  }

  def compositionLaws = composeAssociative && composeDomain && composeCodomain && arrowsContainCompositions

  def identityLaws = identityDomain && identityCodomain && leftIdentity && rightIdentity && arrowsContainIdentities

  def categoryLaws = compositionLaws && identityLaws
}
