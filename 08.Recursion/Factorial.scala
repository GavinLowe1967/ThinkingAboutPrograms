package thinkingAboutPrograms.recursion

/** Object to calculate factorials recursively. */
object Factorial{
  /** The factorial of n, i.e. n!.
    * Pre: n ≥ 0. */
  def fact(n: Int): Int =
    if(n == 0) 1 else n*fact(n-1)

  def main(args: Array[String]) = println(fact(args(0).toInt))
}
