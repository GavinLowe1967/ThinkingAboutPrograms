package thinkingAboutPrograms.recursion

/** Object to perform exponentiation rcursively. */
object Exp{
  /** Returns x^n. Pre: n ≥ 0.  The definition from Section 8.2. */
  def exp(x: Double, n: Long): Double =
    if(n == 0) 1.0
    else if(n%2 == 0) exp(x*x, n/2)
    else x * exp(x*x, n/2)

  /** Returns x^n. Pre: n ≥ 0.  The tail-recursive definition from Section
    *  8.3. */
  def expTR(x: Double, n: Long): Double = exp1(1.0, x, n)

  /** Returns y * z^k . Pre: k ≥ 0. */
  def exp1(y: Double, z: Double, k: Long): Double =
    if(k == 0) y
    else if(k%2 == 0) exp1(y, z*z, k/2)
    else exp1(y*z, z*z, k/2)

  /** Returns x^n. Pre: n ≥ 0.  The version after tail recursion elimination
    *  from Section 8.4. */
  def expTRE(x: Double, n: Long): Double = {
    var y = 1.0; var z = x; var k = n
    while(k != 0){
      if(k%2 == 0){ z = z*z; k = k/2 }
      else{ y = y*z; z = z*z; k = k/2 }
    }
    y
  }

  /** Main function reads arguments from the command line and calls the three
    * implementations. */
  def main(args: Array[String]) = {
    val x = args(0).toDouble; val n = args(1).toLong
    println(exp(x,n))
    println(expTR(x,n))
    println(expTRE(x,n))
  }
}
