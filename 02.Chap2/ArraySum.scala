package thinkingAboutPrograms.arraySum

/** Program to add up a sequence of numbers.  The values to be added should be
  * included on the command line. */
object ArraySum{
  /** The sum of array a.
    * Post: returns sum a[0..n). */
  def findSum(a: Array[Int]): Int = {
    val n = a.size 
    var total = 0; var i = 0
    // Invariant I: total = sum(a[0..i)) && 0<=i<=n
    // Variant n-i
    while(i < n){          // I && i<n
      total = total+a(i)   // total = sum(a[0..i+1)) && i<n
      i = i+1              // I
    } 
    // I && i = n
    // total = sum(a[0..n))
    total
  }

  // Main function. 
  def main(args : Array[String]) = {
    val n = args.size
    val a = new Array[Int](n)
    for(i <- 0 until n) a(i) = args(i).toInt
    println(findSum(a))
  }
}
