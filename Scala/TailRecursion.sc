import scala.annotation.tailrec

//Convert binary number (saved as list of 1 or 0 numbers) to decimal numbers
def binaryToDecimal(binaryList: List[Byte]): Long = {
  @tailrec
  def binaryHelper(list: List[Byte], length: Int, accumulator: Double): Long = list match {
    case Nil => accumulator.toLong
    case h :: t =>
      if (h == 0)
        binaryHelper(t, length - 1, accumulator)
      else if (h == 1)
        binaryHelper(t, length - 1, accumulator + h * math.pow(2.0, (length - 1).toDouble))
      else throw new Exception("Invalid binary number= " + h + ": number must be 0 or 1")
  }

  binaryHelper(binaryList, binaryList.length, 0)
}

//Return chosen by user amount of lucky numbers (sum first of 3 figures are equal sum of last 3 figures
//if number is less figures than 6 then we fill it up to 6 figures by adding 0 on individual number place)
def findLuckyNumbersTail(amountOfNumbersToFind: Int): List[Int] = {
   def valueOfNumberPlace(baseNumber: Int, numberToCalculate: Int): Int =
    if (baseNumber > numberToCalculate) 0
    else
      (numberToCalculate / baseNumber) % 10

  @tailrec
  def luckyNumberHelper(list: List[Int], actualNumber: Int, foundNumbers: Int): List[Int] = {
    if (actualNumber > 999999 || foundNumbers == amountOfNumbersToFind) list;
    else {
      val sumOfFirstThree = valueOfNumberPlace(1, actualNumber) + valueOfNumberPlace(10, actualNumber) + valueOfNumberPlace(100, actualNumber)
      val sumOfSecondThree = valueOfNumberPlace(1000, actualNumber) + valueOfNumberPlace(10000, actualNumber) + valueOfNumberPlace(100000, actualNumber)
      if (sumOfFirstThree == sumOfSecondThree)
        luckyNumberHelper(list ::: List(actualNumber), actualNumber + 1, foundNumbers + 1)
      else luckyNumberHelper(list, actualNumber + 1, foundNumbers)
    }
  }

  if(amountOfNumbersToFind>=0)
    luckyNumberHelper(List(0), 1, 0)
  else Nil
}