package kenbot.yowcat


/**
 * A functor maps one Category to another.
 *
 * It does this by mapping objects to objects, and arrows to arrows.
 *
 * Is is called a "homomorphism" (structure-preserving map), because 
 * the mapping preserves the category features of composition and identity.
 *
 */
trait Functor {
  val dom, cod: Cat

  def mapArr(arr: dom.Arr): cod.Arr 
  def mapObj(obj: dom.Obj): cod.Obj


  // Housekeeping
  override def toString = s"Functor($dom -> $cod)"
  
  override def equals(other: Any): Boolean = other match {
    case f: Functor => functorEquals(this, f)
    case _ => false
  }
}


/**
 * Exercise 4a. (easy) 
 *
 * The simplest functor is of course the identity functor, 
 * which maps a category to itself.
 */
case class IdFunctor(val cat: Cat) extends Functor {
  val dom, cod: cat.type = cat

  def mapArr(arr: cat.Arr): cat.Arr = arr
  def mapObj(obj: cat.Obj): cat.Obj = obj

  override def toString = s"IdFunctor($cat)"
}
