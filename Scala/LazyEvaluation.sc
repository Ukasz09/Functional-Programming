//sieve of Eratosthenes based on lazy evaluation (only wen needed)
val eratosthenesPrimeGenerator = {
  def sieve(lxs: Stream[Int]): Stream[Int] =  lxs match {
      case Stream() => throw new Exception("Internal Error")
      case h #:: tl => h #:: sieve(tl filter (n => n % h != 0))
  }

  sieve(Stream.from(2))
}

//method to genarate any amount of prime numbers
def generatePrimes(amount: Int): List[Int] = {
  if (amount < 0) throw new Exception("Negative arg: amount=" + amount)
  eratosthenesPrimeGenerator.take(amount).toList
}