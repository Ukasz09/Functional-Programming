/*********************************************************************************************************************/
//                                                  Functions
/*********************************************************************************************************************/
def fizBuzz(n: Int): List[String] = {
  def fizBuzzHelper(actual: Int): List[String] = {
    if (actual < n) {
      if (actual % 15 == 0) "FizzBuzz" :: fizBuzzHelper(actual + 1)
      else if (actual % 5 == 0) "Buzz" :: fizBuzzHelper(actual + 1)
      else if (actual % 3 == 0) "Fizz" :: fizBuzzHelper(actual + 1)
      else actual.toString :: fizBuzzHelper(actual + 1)
    } else Nil
  }

  if (n <= 0) Nil
  else fizBuzzHelper(1)
}

def fizBuzz2(n: Int): List[String] = {
  def fizzOrBuzz(number: Int): String = {
    if (number % 15 == 0)
      "FizzBuzz"
    else if (number % 3 == 0)
      "Fizz"
    else if (number % 5 == 0)
      "Buzz"
    else number.toString
  }

  if (n > 0)
    List.range(1, n).map((n: Int) => fizzOrBuzz(n))
  else Nil
}

fizBuzz(100)