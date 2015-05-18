package kenbot.yowcat




/** 
 * Graphs are not categories, because the edges don't compose.  
 *
 * If you take a length of adjoining edges, they do not form
 * an edge from the start to the end.
 */
trait Graph {
  type Node 
  type Edge = (Node, Node)

  def edges: Stream[Edge]
  def nodes: Stream[Node] = edges.flatMap(e => Stream(e._1, e._2))
}


/**
 * Exercise 2. (hard)
 *
 * Implement a Free Category from a Graph.
 *
 * We can generate a Free Category by looking at 
 * paths of adjoining edges instead of the edges themselves! 
 *
 * To keep it sane, let's omit looping paths. 
 */
class FreeCategory(val graph: Graph) extends Cat {
  type Edge = graph.Edge

  type Obj = graph.Node 
  def objects: Stream[Obj] = graph.nodes


  // Path is reversed: D <-- C <-- B <-- A = START
  case class Arr(start: Obj, path: Stream[Edge]) {
    def end: Obj = path match {
      case Stream.Empty => start
      case end #:: _ => end._2
    }

    def ++(arr: Arr): Arr = Arr(start, arr.path #::: path)

    def +:(e: Edge): Arr = Arr(start, e #:: path)

    def isFollowedBy(e: Edge): Boolean = e._1 == end
  }

  // All paths formed by edges in the graph!
  def arrows: Stream[Arr] = {

    def noLoops(e: Edge, arr: Arr) = arr.path.forall { 
      case (from, to) => e._2 != from && e._2 != to 
    }

    def expand(path: Arr): Stream[Arr] = { 
      graph.edges.collect {
        case e if path.isFollowedBy(e) && 
                  noLoops(e, path) => e +: path
      }
    }

    def loop(lastBatch: Stream[Arr], results: Stream[Arr]): Stream[Arr] = {
      lastBatch.flatMap(expand) match {
        case Stream.Empty => results 
        case next => loop(next, results #::: next) 
      }
    }

    val ids = graph.nodes.map(id) 
    loop(ids, ids)
  }

  def dom(arr: Arr): Obj = arr.start
  def cod(arr: Arr): Obj = arr.end 

  def comp(g: Arr, f: Arr): Arr = f ++ g
  def id(obj: Obj): Arr = Arr(obj, Stream())
}
