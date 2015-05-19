package kenbot

package object yowcat {

  type ??? = Nothing

  def catToString(name: String, cat: Cat): String = {
    def longerThan(seq: Seq[_], n: Int) = seq.take(n+1).size > seq.take(n).size 
    def ellipsisString(seq: Seq[_], n: Int) = {
      val str = seq.take(n).mkString(", ")
      if (longerThan(seq,n)) s"$str, ..." else str
    }
    def isMonoid = cat.objects == Stream(())
    def arrToString(f: cat.Arr) = { 
      if (isMonoid) f.toString
      else s"${cat.dom(f)} -> ${cat.cod(f)}"
    }
    def objStrings = ellipsisString(cat.objects, 20)     
    def arrStrings = ellipsisString(cat.arrows.map(arrToString), 20)

    s"""$name { 
      |  objs = { $objStrings }
      |  arrs = { $arrStrings }
      |}""".stripMargin
  }



  /**
   * Reasonable facsimile of category equality.
   * Just checks a sample of objects, arrows and 
   * composed arrows for equality. 
   */
  def catEquals(cat1: Cat, cat2: Cat): Boolean = {
    import org.scalacheck._
    import org.scalacheck.Prop._

    val catGen1 = new CatGenerators[cat1.type](cat1)
    val catGen2 = new CatGenerators[cat2.type](cat2)

    implicit val arbObj1 = catGen1.arbObj 
    implicit val arbObj2 = catGen2.arbObj
    implicit val arbArr1 = catGen1.arbArr
    implicit val arbArr2 = catGen2.arbArr
    implicit val composable1 = catGen1.arbComposable2
    implicit val composable2 = catGen2.arbComposable2

    val objectsSameish: Prop = forAll {
      (o1: cat1.Obj, o2: cat2.Obj) => cat2.objects.contains(o1) && cat1.objects.contains(o2) 
    }

    val arrowsSameish: Prop = forAll {
      (a1: cat1.Arr, a2: cat2.Arr) => cat2.arrows.contains(a1) && cat1.arrows.contains(a2) 
    }

    val compositionSameish: Prop = forAll {
      (cmp1: catGen1.Composable2, cmp2: catGen2.Composable2) => 
        val catGen1.Composable2(g1, f1) = cmp1
        val catGen2.Composable2(g2, f2) = cmp2

        def compEqual(c1: Cat, c2: Cat)(g: c1.Arr, f: c1.Arr) = 
          c1.compose(g,f) == c2.compose(g.asInstanceOf[c2.Arr], f.asInstanceOf[c2.Arr])

        compEqual(cat1, cat2)(g1, f1) && compEqual(cat2, cat1)(g2, f2)
    }

    val equalish = objectsSameish && arrowsSameish && compositionSameish
    Test.check(Test.Parameters.default, equalish).status == Test.Passed
  }


  /**
    * Reasonable facsimile of functor equality; we just take a sample of 
    * mappings Obj -> Obj and Arr -> Arr, and check they have the same result.
    */
  def functorEquals(f1: Functor, f2: Functor): Boolean = {
    import org.scalacheck._
    import org.scalacheck.Prop._

    val sameHomSet = f1.dom == f2.dom && f1.cod == f2.cod

    sameHomSet && {
      val dom1Gen = new CatGenerators[f1.dom.type](f1.dom)
      val dom2Gen = new CatGenerators[f2.dom.type](f2.dom)

      implicit val arbDomObj1 = dom1Gen.arbObj
      implicit val arbDomObj2 = dom2Gen.arbObj
      implicit val arbDomArr1 = dom1Gen.arbArr
      implicit val arbDomArr2 = dom2Gen.arbArr

      val objectsMappedSameish: Prop = forAll {
        (o1: f1.dom.Obj, o2: f2.dom.Obj) => 
          f1.mapObj(o1) == f2.mapObj(o1.asInstanceOf[f2.dom.Obj]) && 
          f2.mapObj(o2) == f1.mapObj(o2.asInstanceOf[f1.dom.Obj])
      }

      val arrowsMappedSameish: Prop = forAll {
        (a1: f1.dom.Arr, a2: f2.dom.Arr) => 
          f1.mapArr(a1) == f2.mapArr(a1.asInstanceOf[f2.dom.Arr]) && 
          f2.mapArr(a2) == f1.mapArr(a2.asInstanceOf[f1.dom.Arr])
      }

      val equalish = objectsMappedSameish && arrowsMappedSameish
      Test.check(Test.Parameters.default, equalish).status == Test.Passed
    }
  }

}
