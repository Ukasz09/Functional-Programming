//Return lazy list with consecutives fibbonaci numbers
val lazyFibNumbers: LazyList[BigInt] =
  BigInt(0) #:: BigInt(1) #:: lazyFibNumbers.zip(lazyFibNumbers.tail).map(n => n._1 + n._2)
