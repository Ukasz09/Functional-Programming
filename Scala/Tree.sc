import scala.annotation.tailrec

//binary tree ADT
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

//return size of binary tree (total number of nodes)
def size[A](binaryTree: BT[A]): Int = binaryTree match {
    case Empty => 0
    case Node(_, leftTree, rightTree) => 1 + size(leftTree) + size(rightTree)
}

//return maximum depth (number of nodes along the longest path from the root node down to the farthest leaf node)
def maxDepth[A](binaryTree: BT[A]): Int = {
  def maxDepthHelper[A](binaryTree: BT[A], actualDepth: Int): Int = binaryTree match {
    case Empty => actualDepth
    case Node(_, leftTree, rightTree) =>
      val rightDepth =
        if (rightTree != Empty) maxDepthHelper(rightTree, actualDepth + 1)
        else actualDepth

      val leftDepth =
        if (leftTree != Empty) maxDepthHelper(leftTree, actualDepth + 1)
        else actualDepth

      if (leftDepth > rightDepth)
        leftDepth
      else rightDepth
  }
  maxDepthHelper(binaryTree, 0)
}

//map function for binary tree 
def map[A, B](binaryTree: BT[A], function: A => B): BT[B] = binaryTree match {
    case Empty => Empty
    case Node(value, leftTree, rightTree) =>
      Node(function(value), map(leftTree, function), map(rightTree, function))
}

//fold (based on level order traversal/breadth first traversal)
def fold[A,B](tree: BT[A])(acc: B)(function: (B, A) => B): B = {
  @tailrec
  def helper(nodeQueue: List[BT[A]], acc: B): B = nodeQueue match {
      case Nil => acc
      case Empty :: tail => helper(tail, acc)
      case Node(value, leftSubtree, rightSubtree) :: tail =>
        helper(tail ::: List(leftSubtree, rightSubtree), function(acc, value))
    }
  helper(List(tree), acc)
}
