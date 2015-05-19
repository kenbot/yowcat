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

  type Obj = ???
  def objects: Stream[Obj] = ???

  type Arr = ???

  // All paths formed by edges in the graph!
  def arrows: Stream[Arr] = ??? 

  def dom(arr: Arr): Obj = ???
  def cod(arr: Arr): Obj = ???

  def comp(g: Arr, f: Arr): Arr = ???
  def id(obj: Obj): Arr = ???
}
