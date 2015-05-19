package kenbot.yowcat

import org.scalacheck._
import org.scalacheck.Prop._


object Exercise5bSpec extends Properties("Exercise 5b: Monoid Homomorphisms as functors") {

  import Exercise5._

  class MonoidHomTests[M, N](hom: MonoidHom[M, N]) {

    val monoidM: Monoid[M] = hom.dom
    val monoidN: Monoid[N] = hom.cod 

    val mGen = new CatGenerators(monoidM)
    implicit val arbM: Arbitrary[M] = mGen.arbArr

    val mId = monoidM.id(())
    val nId = monoidN.id(())

    import hom.{mapArr, mapObj}

    property("Mapping from M -> N preserves identity element") = 
      Prop(nId == mapArr(mId))

    property("Mapping from M -> N preserves composition") = forAll {
      (f: M, g: M) => 
        mapArr(monoidM.compose(g,f)) == monoidN.compose(mapArr(g), mapArr(f)) 
    }
  }

  val strAppend = Monoid(Sets.strings)(_ + _, "")
  val intAdd = Monoid(Sets.ints)(_ + _, 0)
  val unitMonoid = Monoid(Sets.unit)((_,_) => (), ())
  new MonoidHomTests(MonoidHom[String,Int](strAppend, intAdd, _.length))
  new MonoidHomTests(MonoidHom[Int,Unit](intAdd, unitMonoid, _ => ()))
}
