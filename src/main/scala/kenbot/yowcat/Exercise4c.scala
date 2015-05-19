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
  def arrows: Stream[Functor] = ???

  def dom(f: Functor): Cat = ???
  def cod(f: Functor): Cat = ???

  /** 
   * Because this representation of a category sacrifices type safety so that we can 
   * manipulate objects as values, you will need to perform a type cast here 
   * to make the arrows click together.
   *
   * This is obviously unnecessary in the usual type-safe usage.
   * 
   * Syntax: foo.asInstanceOf[Bar] 
   */
  override def comp(f: Functor, g: Functor): Functor = ??? 

  def id(cat: Cat): Functor = ???
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
