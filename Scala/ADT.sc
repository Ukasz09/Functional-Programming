import scala.annotation.tailrec

//Very simple calculator's operation ADT
sealed trait Operation
final case class Addition(a: Operation, b: Operation) extends Operation
final case class Subtraction(a: Operation, b: Operation) extends Operation
final case class Multiplication(a: Operation, b: Operation) extends Operation
final case class Division(a: Operation, b: Operation) extends Operation
final case class Negation(n: Operation) extends Operation
final case class Value(n: Double) extends Operation

def simpleCalculator(operation: Operation): Double = operation match {
  case Addition(a: Operation, b: Operation) => simpleCalculator(a) + simpleCalculator(b)
  case Subtraction(a: Operation, b: Operation) => simpleCalculator(a) - simpleCalculator(b)
  case Multiplication(a: Operation, b: Operation) => simpleCalculator(a) * simpleCalculator(b)
  case Division(a: Operation, b: Operation) => {
    val numb_B = simpleCalculator(b)
    if (numb_B != 0) simpleCalculator(a) / simpleCalculator(b)
    else throw new Exception("Can NOT divide by 0 !")
  }
  case Negation(n) => -simpleCalculator(n)
  case Value(n) => n
}

//Own boolean implementation usd ADT
sealed trait Bool
case object True extends Bool
case object False extends Bool

def and(bool1: Bool, bool2: Bool): Bool = {
  (bool1, bool2) match {
    case (True, True) => True
    case (_, _) => False
  }
}

def or(bool1: Bool, bool2: Bool): Bool = {
  if(bool1==True || bool2==True)
    True
  else False
}

def not(bool: Bool): Bool = if (bool == True) False else True

def xor(bool1: Bool, bool2: Bool): Bool = {
  (bool1, bool2) match {
    case (False, False) | (True, True) => False
    case (_, _) => True
  }
}