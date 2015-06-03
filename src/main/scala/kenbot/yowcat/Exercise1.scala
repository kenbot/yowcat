package kenbot.yowcat


/**
 * Lets start with this representation of a category.
 *
 * Ordinarily we'd make better use of the type system here, 
 * but we want to manipulate all these concepts at the value level.
 * 
 * Throughout, we'll use scala.Stream to represent sets or collections
 * in the mathematical sense, even though they don't guarantee uniqueness.
 *
 * To keep things simple, we'll use the standard JVM equality operator '=='
 * to compare things.
 * 
 */
trait Cat {

  /** 
   * 1. Objects 
   * Can represent anything at all.  
   */
  type Obj
  def objects: Stream[Obj]

  /**
   * 2. Arrows
   * Can represent anything at all.
   */
  type Arr
  def arrows: Stream[Arr]

  /**
   * 3. Get the domain of an arrow; the "A" in "A -> B"
   */
  def dom(f: Arr): Obj

  /**
   * 4. Get the codomain of an arrow; the "B" in "A -> B"
   */
  def cod(f: Arr): Obj


  /** 
   * 5. Composition 
   * 
   * Arguments:
   *    f      g
   * A ---> B ---> C
   *
   * Returning:
   * A ----------> C
   *
   * Must be associative: 
   * compose(a, compose(b,c)) == compose(compose(a,b),c)  
   */
  final def compose(g: Arr, f: Arr): Arr = { 
    assert(canCompose(g, f))
    comp(g, f) 
  }

  protected[this] def comp(g: Arr, f: Arr): Arr

  final def canCompose(g: Arr, f: Arr): Boolean = 
    cod(f) == dom(g)


  /**
   * 6. Identity
   * Must be neutral when composed from the left or right:
   * compose(id, a) == compose(a, id) == a
   */
  def id(obj: Obj): Arr


  // Housekeeping
  override def toString() = catToString("Cat", this)

  override def equals(o: Any) = o match {
    case o: Cat => catEquals(this, o)
    case _ => false
  }
}


/** 
 * Exercise 1: 
 *
 * Implement the category of JVM Classes and subtype-relationships.
 *
 * Let's only use this hierarchy for our purposes:
 *
 * Food
 *   +-- Fruit
 *   |     +-- Banana
 *   |     +-- Cumquat
 *   |     +-- Grape
 *   +-- Meat
 *         +-- Kangaroo
 *         +-- Yak
 *         +-- Goat
 *
 * Tool
 *   +-- Hammer
 *   +-- Spanner
 *
 *
 * What are the objects, and what are the arrows?
 */
object Exercise1 {


  // For convenience, let's import a 
  // 'a.isSubTypeOf(b)' extension method for Class[_].
  import SubTypeSugar._

  // Usage:
  // classOf[Grape].isSubTypeOf(classOf[Fruit])
  // =====> true
  //
  // classOf[Spanner].isSubTypeOf(classOf[Banana])
  // =====> false


  // For convenience, let's just use these classes as the set of "all the classes".
  val setOfClasses: Stream[Class[_]] = Stream(
    classOf[Food], classOf[Fruit], classOf[Banana], 
    classOf[Cumquat], classOf[Grape], classOf[Meat], 
    classOf[Yak], classOf[Goat], classOf[Kangaroo], 
    classOf[Tool], classOf[Spanner], classOf[Hammer])

  
  object ClassesSubtypesCategory extends Cat {

    type Obj = Class[_] 
    
    // For argument's sake, let's just say that 
    // these are the only Classes that exist:
    def objects: Stream[Obj] = setOfClasses

    /** 
     * Exercise 1a. 
     * 
     * How could we represent the arrows - subtype relations 
     * between classes?
     *
     * How can we construct the set (ie Stream) of them? 
     */
    type Arr = ???
    def arrows: Stream[Arr] = ???

    /** 
     * Exercise 1b. 
     * 
     * How can we get the domain and codomain objects for an arrow?
     */
    def dom(f: Arr): Class[_] = ???
    def cod(f: Arr): Class[_] = ???

    /**
     * Exercise 1c. Identity 
     *
     * Produce a representation of the fact that 
     * a class is its own subtype.
     *
     * Composing with any other subtyping relationship must just
     * return the other one.
     */
    def id(obj: Class[_]): Arr = ???

    /**
     * Exercise 1d. Composition 
     *
     * How can we compose our representation of 
     * subtype relationships between classes?
     *
     * Arguments:
     *    f      g
     * A ---> B ---> C
     *
     * Returning:
     * A ----------> C
     * 
     * (Don't forget to make it associative!)
     */
    def comp(g: Arr, f: Arr): Arr = ???

  }


  /**
   * Exercise 1e. Reimplement the category above so that it can describe any 
   * partially ordered set, as represented by a Stream[S] and the given ordering function. 
   */
  class PosetCategory[S](set: Stream[S], orderingFn: (S,S) => Boolean) extends Cat {
    final def order(a: Obj, b: Obj): Boolean = orderingFn(a,b)

    type Obj = S
    def objects: Stream[Obj] = set

    type Arr = ???
    def arrows: Stream[Arr] = ???

    def dom(f: Arr): Obj = ???
    def cod(f: Arr): Obj = ???

    def comp(g: Arr, f: Arr): Arr = ???
    def id(obj: Obj): Arr = ???
  }
}
