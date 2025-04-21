package thinkingAboutPrograms.binarySearch

/** Object to find integer square roots using binary search. */
object SquareRoots{
  /** The integer square root of y. */
  def binarySqrt(y: Int): Int = {
    require(y >= 0)
    // Invariant I = a^2 ≤ y < b^2 ∧ 0 ≤ a < b.
    var a = 0; var b = y+1
    while(a+1 < b){ // I ∧ a+1 < b.
      val m = (a+b)/2 // a < m < b.
      if(m*m <= y) a = m // I.
      else b = m // I.
    }
    // I ∧ b = a+1 so a^2 ≤ y < (a+1)^2.
    a
  }

  def main(args: Array[String]) = println(binarySqrt(args(0).toInt))
}
