package kenbot.yowcat

object SubTypeSugar {

  // Let's introduce some syntax sugar for testing
  // a subtype relation; a.isSubTypeOf(b)
  implicit class ClassOps(thisClass: Class[_]) {
    def isSubTypeOf(other: Class[_]): Boolean = 
      other.isAssignableFrom(thisClass)
  }
}

trait Food

trait Fruit extends Food
class Banana extends Fruit
class Cumquat extends Fruit
class Grape extends Fruit

trait Meat extends Food
class Yak extends Meat
class Goat extends Meat
class Kangaroo extends Meat


trait Tool
class Spanner extends Tool
class Hammer extends Tool

