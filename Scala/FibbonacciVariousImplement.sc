import scala.annotation.tailrec

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                              Normal recursion
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

def fibNumberIndex(amount: Int): BigInt = {
  if (amount < 0)
    throw new Exception("Negative argument amount=" + amount)

  def fibHelper(index: Int): BigInt = index match {
    case 0 => BigInt(0);
    case 1 => BigInt(1);
    case n: Int => fibHelper(n - 2) + fibHelper(n - 1)
  }

  fibHelper(amount)
}

def fibNumberList(amount: Int): List[BigInt] = {
  if (amount < 0)
    throw new Exception("Negative argument amount=" + amount)

  def fibHelper(n: Int): List[BigInt] = {
    if (n == amount) Nil
    else fibNumberIndex(n) :: fibHelper(n + 1)
  }

  fibHelper(0)
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                              Tail recursion
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//Return list with chosen amount of first Fibbonacci numbers in reverse order, by using tail recursion
def tailFibNumberListRevers(amount: Int): List[BigInt] = {
  if (amount < 0)
    throw new Exception("Negative argument amount=" + amount)

  @tailrec
  def fibHelper(n: Int, actualNumber: BigInt, nextNumber: BigInt, fibList: List[BigInt]): List[BigInt] = {
    if (n == 0) fibList
    else fibHelper(n - 1, nextNumber, actualNumber + nextNumber, actualNumber :: fibList)
  }

  fibHelper(amount, 0, 1, List())
}

//Return list with chosen amount of first Fibbonacci numbers by using tail recursion
def tailFibNumberList(amount: Int): List[BigInt] = {
  if (amount < 0)
    throw new Exception("Negative argument amount=" + amount)
  tailFibNumberListRevers(amount).reverse
}

//Return chosen index of Fibbonacci numbers
def tailFibNumberIndex(index: Int): BigInt = {
  if (index < 0)
    throw new Exception("Negative argument index=" + index)
  tailFibNumberListRevers(index + 1).head
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                             Lazy evaluated list
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//Return lazy evaluated list with consecutives Fibbonacci numbers
val lazyListFib: LazyList[BigInt] =
  BigInt(0) #:: BigInt(1) #:: lazyListFib.zip(lazyListFib.tail).map(n => n._1 + n._2)

//Return list with chosen amount of first Fibbonacci numbers by using lazy evaluation
def lazyFibNumberList(amount: Int): List[BigInt] = {
  if (amount < 0)
    throw new Exception("Negative argument amount=" + amount)

  lazyListFib.take(amount).toList
}

//Return chosen index of Fibbonacci numbers by using lazy evaluation
def lazyFibNumberIndex(index: Int): BigInt = {
  if (index < 0)
    throw new Exception("Negative argument index=" + index)
  lazyListFib.apply(index)
}