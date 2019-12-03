/**********************************************************************************************************************/
//                                                  Functions
/*********************************************************************************************************************/

//Apply function to every element of two passed lazy lists (function(hd1,hd2)) and return result as lazy lists
def lAction[A](function: (A, A) => A)(llist1: Stream[A], llist2: Stream[A]): Stream[A] = (llist1, llist2) match {
  case (Stream(), Stream()) => Stream()
  case (Stream(), Stream.cons(hd, tl)) => hd #:: lAction(function)(Stream(), tl)
  case (Stream.cons(hd, tl), Stream()) => hd #:: lAction(function)(Stream(), tl)
  case (Stream.cons(hd1, tl1), Stream.cons(hd2, tl2)) =>
    function(hd1, hd2) #:: lAction(function)(tl1, tl2)
}