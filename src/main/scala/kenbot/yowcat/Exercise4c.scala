package kenbot.yowcat


/**
 * Exercise 4c. 
 *
 * It's turtles all the way down!
 *
 * In the category of (small[1]) categories, functors themselves are the arrows!
 *
 * Let's build it. To keep it simple, lets just use Cat1 and Cat2, from Exercise 4b. 
 */
object CatCat extends Cat {
  type Obj = Cat
  override def objects: Stream[Cat] = Stream(Cat1, Cat2)

  type Arr = Functor
  def arrows: Stream[Functor] = Cat1Cat2Functor #:: objects.map(id)

  def dom(f: Functor): Cat = f.dom
  def cod(f: Functor): Cat = f.cod

  /** 
   * Because this representation of a category sacrifices type safety so that we can 
   * manipulate objects as values, you will need to perform a type cast here 
   * to make the arrows click together.
   *
   * This is obviously unnecessary in the usual type-safe usage.
   * 
   * Syntax: foo.asInstanceOf[Bar] 
   */
  override def comp(
      f: Functor,
      g: Functor): Functor = new Functor {

    val dom: g.dom.type = g.dom
    val cod: f.cod.type = f.cod

    // Assuming that f.dom == g.cod. 
    def mapArr(arr: g.dom.Arr): f.cod.Arr =
      f.mapArr(g.mapArr(arr).asInstanceOf[f.dom.Arr])

    def mapObj(obj: g.dom.Obj): f.cod.Obj =
      f.mapObj(g.mapObj(obj).asInstanceOf[f.dom.Obj])
  }

  def id(cat: Cat): Functor = new IdFunctor(cat)
}

/* [1]
 *
 * A "small category" means that the objects form a Set, and the arrows form a Set.
 *
 * In particular, the category of Sets is a "large category" 
 * (because there is no such thing as a Set of Sets). Our "category of categories" here is
 * also a large category -- and so thankfully, doesn't include itself.
 * 
 * Such is the price of dodging paradoxes! We needn't let this foundational ducking and weaving 
 * bother us for these exercises though.
 */
