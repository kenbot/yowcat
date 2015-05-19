package kenbot.yowcat



// We'll use scala.Streams as a stand-in for Sets,
// even though they don't prevent duplicates.
object Sets {
  def ints: Stream[Int] = Stream.iterate(0) { n => 
    if (n.signum != 1) -n + 1 
    else -n
  }

  def positiveNats: Stream[Int] = Stream.iterate(1)(_ + 1)

  def nats: Stream[Int] = Stream.iterate(0)(_ + 1)

  def booleans: Stream[Boolean] = Stream(true, false)

  def unit: Stream[Unit] = Stream(())

  def classes: Stream[Class[_]] = Stream(
    classOf[Food], classOf[Fruit], classOf[Banana], 
    classOf[Cumquat], classOf[Grape], classOf[Meat], 
    classOf[Yak], classOf[Goat], classOf[Kangaroo], 
    classOf[Tool], classOf[Spanner], classOf[Hammer])

  import Exercise1.PosetCategory

  def posets: Stream[PosetCategory[_]] = {
    import SubTypeSugar._

    val classHierarchy = new PosetCategory[Class[_]](classes, _ isSubTypeOf _)

    def fooPoset: PosetCategory[_] = {
      case class Foo(n: Int, under: Option[Int] = None) {
        def isUnder(f: Foo) = f.n == n || under.contains(f.n) 
      }

      val foos = Stream(Foo(1), Foo(2, Some(1)), Foo(3, Some(2)), Foo(4))

      new PosetCategory[Foo](foos, _ isUnder _)
    }

    Stream(classHierarchy, fooPoset)
  }

  def lists[A](elements: Stream[A]): Stream[List[A]] = {
    def listsOfLength(n: Int): Stream[List[A]] = { 
      if (n == 0) Stream(Nil)
      else for {
        list <- listsOfLength(n-1) 
        e <- elements
      } yield e :: list 
    }
    nats.flatMap(listsOfLength)
  }
}



