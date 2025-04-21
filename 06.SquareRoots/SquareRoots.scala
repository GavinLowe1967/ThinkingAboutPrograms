package thinkingAboutPrograms.squareRoots

object SquareRoots{
  /** Integer square root of y .
    * Pre: y ≥ 0.
    * Post: returns a such that a^2 ≤ y < (a+1)^2. */
  def linearSqrt(y: Int) : Int = {
    require(y >= 0)
    var a = 0; var m = 1 // Invariant I = a^2 <= y && m = (a+1)^2
    while(m <= y){ a += 1; m = m+a+a+1 }
    // a^2 ≤ y < (a+1)^2 .
    a
  }

  def main(args: Array[String]) = println(linearSqrt(args(0).toInt))
}
