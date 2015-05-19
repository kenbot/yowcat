package kenbot.yowcat


/**
 * Exercise 4b. 
 *
 * Write a functor between Cat1 and Cat2; making 
 * sure that you preserve identity and composition.
 *
 * There are several correct (and incorrect) ways to do this.
 */
object Cat1Cat2Functor extends Functor {
  val dom = Cat1
  val cod = Cat2

  def mapArr(arr: (XYZ, XYZ)): (Color, Color) = ???

  def mapObj(obj: XYZ): Color = ??? 
}


sealed trait XYZ 
case object X extends XYZ
case object Y extends XYZ
case object Z extends XYZ

sealed trait Color
case object Red extends Color
case object Green extends Color
case object Blue extends Color
case object Yellow extends Color
case object White extends Color
case object Black extends Color
case object Grey extends Color

object Cat1 extends Cat {
  type Obj = XYZ
  def objects = Stream(X,Y,Z)

  type Arr = (Obj, Obj)
  def arrows = Stream(
    X -> X, X -> Y, X -> Z, 
    Y -> Y, Y -> Z, 
    Z -> Z)

  def dom(f: Arr): Obj = f._1
  def cod(f: Arr): Obj = f._2

  def comp(g: Arr, f: Arr): Arr = (f._1, g._2) 
  def id(a: Obj): Arr = (a, a)

}

object Cat2 extends Cat {
  type Obj = Color
  def objects = Stream(Red, Green, Blue, Yellow, White, Black, Grey)

  type Arr = (Obj, Obj) 
  def arrows = Stream(
    Red -> Red, Red -> Black,
    Blue -> Blue, Blue -> Green, Blue -> Yellow,
    Green -> Green, Green -> Yellow,
    Yellow -> Yellow, 
    White -> White, White -> Red, White -> Black, 
    Black -> Black, 
    Grey -> Grey)

  def dom(f: Arr): Obj = f._1
  def cod(f: Arr): Obj = f._2

  def comp(g: Arr, f: Arr): Arr = (f._1, g._2) 
  def id(a: Obj): Arr = (a, a)
}



