package thinkingAboutPrograms.polynomial 

/** Code to evaluate a polynomial. */
object Polynomial{
  /** Evaluate the polynomial represented by `a` at `x`. */
  def evaluate(a: Array[Double], x: Double): Double = {
    val len = a.length; var k = 0; var sum = 0.0; var y = 1.0
    // Invariant sum = sum_{i \in [0..k)} a(i) * x^i && y = x^k && 0 <= k <= len
    while(k < len){
      sum += a(k)*y; y *= x; k += 1
    }
    sum
  }

  /** Evaluate the polynomial represented by `a` at `x`, using Horner's rule. */
  def horners(a: Array[Double], x: Double): Double = {
    val len = a.length; var k = len; var sum = 0.0
    while(k > 0){
      k -= 1
      sum = sum*x + a(k)      // invariant re-established
    }
    sum
  }

  /** Evaluate a polynomial based on values provided on the command line.  All
    * but the last argument should represent the coefficients of the
    * polynomial; the last argument should give the value at which to eveluate
    * the polynomial. */
  def main(args: Array[String]) = {
    val a = args.init.map(_.toDouble); val x = args.last.toDouble
    println(evaluate(a, x))
    println(horners(a, x))
  }

}
