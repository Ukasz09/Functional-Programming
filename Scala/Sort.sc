def quickSort[A](comparator: (A, A) => Boolean, list: List[A]): List[A] = list match {
  case Nil => Nil
  case h :: t => {
    val smaller = t.filter((y: A) => comparator(y, h))
    val bigger = t.filter((y: A) => !comparator(y, h))
    quickSort(comparator, smaller) ::: (h :: quickSort(comparator, bigger))
  }
}

def insertSort[A](comparator: (A, A) => Boolean, list: List[A]): List[A] = {
  def insert(element: A, newList: List[A]): List[A] = newList match {
    case Nil => element :: Nil
    case h :: t =>
      if (!comparator(h, element)) element :: newList
      else h :: insert(element, t)
  }

  list.foldLeft(List[A]())((acc, newElement) => insert(newElement, acc))
}

def mergeSort[A](comparator: (A, A) => Boolean, list: List[A]): List[A] = {
  def merge(list1: List[A], list2: List[A]): List[A] = (list1, list2) match {
    case (Nil, list2) => list2
    case (list1, Nil) => list1
    case (h1 :: t1, h2 :: t2) =>
      if (comparator(h1, h2)) h1 :: merge(t1, list2)
      else h2 :: merge(list1, t2)
  }

  val partition = list.length / 2
  if (partition == 0) list
  else {
    val (left, right) = list.splitAt(partition)
    merge(mergeSort(comparator, left), mergeSort(comparator, right))
  }
}