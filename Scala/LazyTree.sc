import scala.annotation.tailrec

//Binary tree ADT
sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]

//Return size of binary tree (total number of nodes)
def size[A](binaryTree: lBT[A]): Int = binaryTree match {
  case LEmpty => 0
  case LNode(_, leftTree, rightTree) => 1 + size(leftTree()) + size(rightTree())
}

//Return maximum depth (number of nodes along the longest path from the root node, down to the farthest leaf node)
def maxDepth[A](binaryTree: lBT[A]): Int = {
  def maxDepthHelper(binaryTree: lBT[A], actualDepth: Int): Int = binaryTree match {
    case LEmpty => actualDepth
    case LNode(_, leftTree, rightTree) =>
      val rightDepth =
        if (rightTree() != LEmpty) maxDepthHelper(rightTree(), actualDepth + 1)
        else actualDepth

      val leftDepth =
        if (leftTree() != LEmpty) maxDepthHelper(leftTree(), actualDepth + 1)
        else actualDepth

      if (leftDepth > rightDepth)
        leftDepth
      else rightDepth
  }

  maxDepthHelper(binaryTree, 0)
}

//Map function for binary tree
def map[A, B](binaryTree: lBT[A], function: A => B): lBT[B] = binaryTree match {
  case LEmpty => LEmpty
  case LNode(value, leftTree, rightTree) =>
    LNode(function(value), () => map(leftTree(), function), () => map(rightTree(), function))
}

//Fold (based on level order traversal/breadth first traversal)
def fold[A, B](tree: lBT[A])(acc: B)(function: (B, A) => B): B = {
  @tailrec
  def helper(nodeQueue: List[lBT[A]], acc: B): B = nodeQueue match {
    case Nil => acc
    case LEmpty :: tail => helper(tail, acc)
    case LNode(value, leftSubtree, rightSubtree) :: tail =>
      helper(tail ::: List(leftSubtree(), rightSubtree()), function(acc, value))
  }

  helper(List(tree), acc)
}

//level order traversal/breadth first traversal, returned as LazyList
def BFS[A](tree: lBT[A]): LazyList[A] = {
  def search(nodesQueueList: List[lBT[A]]): LazyList[A] = nodesQueueList match {
    case Nil => LazyList()
    case LEmpty :: tail => search(tail)
    case LNode(value, leftSubtree, rightSubtree) :: tail =>
      value #:: (search(tail ::: List(leftSubtree(), rightSubtree())))
  }

  search(List(tree))
}
