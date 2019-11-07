sealed trait Graphs[A]
case class Graph[A](succ: A => List[A]) extends Graphs[A]

//Breadth-first search (BFS) algorithm for traversing graph
def breadthSearch[A](graph: Graph[A])(startNode: A): List[A] = {
  def search(visited: List[A])(queue: List[A]): List[A] = queue match {
    case Nil => Nil;
    case h :: t =>
      if (visited.contains(h)) search(visited)(t)
      else h :: search(h :: visited)(t ::: (graph succ h))
  }

  search(Nil)(List(startNode))
}

//Depth-first search (DFS) algorithm for traversing graph
def depthSearch[A](graph: Graph[A])(startNode: A): List[A] = {
  def search(visited: List[A])(queue: List[A]): List[A] = queue match {
    case Nil => Nil;
    case h :: t =>
      if (visited.contains(h)) search(visited)(t)
      else h :: search(h :: visited)((graph succ h) ::: t)
  }

  search(Nil)(List(startNode))
}