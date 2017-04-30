// Exercise 1
def toDigits(x: Long): List[Long] = {
  def toDigitList(lst: List[Char]): List[Long] = 
    lst match {
      case Nil => Nil
      case x :: xs => x.asDigit :: toDigitList(xs)
    }
  x match {
    case n if n <= 0 => Nil
    case n => toDigitList(x.toString.toList)
  }
}

def toDigitsRev(x: Long): List[Long] = 
  toDigits(x).reverse
// End Exercise 1

// Exercise 2
def doubleEveryOther(Longs: List[Long]): List[Long] =
  Stream.from(1).
    zip(Longs.reverse).
    map(x => if (x._1 % 2 == 0) x._2 * 2 else x._2).
    toList.
    reverse
// End Exercise 2

// Exercise 3
def sumDigits(Longs: List[Long]): Long = 
  Longs.flatMap(toDigits).sum
// End Exercise 3


// Exercise 4
def validate(x: Long): Boolean = {
  val sumAll: Long => Long = 
    (toDigits _) andThen
    doubleEveryOther andThen
    sumDigits

  x match {
    case n if n <= 0 => false
    case n => if (sumAll(n) % 10 == 0) true else false
  }
}

// End Exercise 4


// Exercise 5
type Peg = String
type Move = (Peg, Peg)
def hanoi(n: Int, a: Peg, b: Peg, c: Peg): List[Move] = 
  n match {
    case 0 => Nil
    case n => hanoi(n - 1, a, c, b) ++ 
              List((a, b)) ++ 
              hanoi(n - 1, c, b, a)
  }

// End Exercise 5


// Exercise 6 (Optional)
def hanoi4(n: Int, a: Peg, b: Peg, c: Peg, d: Peg): List[Move] = 
  n match {
    case 0 => Nil
    case n => hanoi4(n / 2, a, c, b, d) ++ 
              hanoi(n - (n/2), a, b, d) ++
              hanoi4(n / 2, c, b, a, d)
  }
// End Exercise 6

