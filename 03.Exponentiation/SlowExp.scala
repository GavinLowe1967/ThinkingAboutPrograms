package thinkingAboutPrograms.exponentiation

/** An inefficient iterative definition of exponentiation. */
object SlowExp{
  /** Calculate x^n
    * Precondition: n >= 0. */
  def exp(x: Double, n: Long) : Double = {
    // Invariant I: y = x^i && i <= n
    require(n >= 0)
    var y = 1.0; var i = 0L // I
    while(i < n){         // I && i < n
      y = y*x; i = i+1  // I 
    }
    // I && i = n, so y = x^n
    y
  }

  def main(args:Array[String]) = {
    if(args.length != 2) println("Usage: scala SlowExp x n")
    else{
      val t0 = java.lang.System.nanoTime
      val x = args(0).toDouble
      val n = args(1).toLong
      if(n>=0) println(x.toString+"^"+n+" = "+exp(x,n))
      else println("Error: second argument should be non-negative")
      println("Time taken: "+(java.lang.System.nanoTime-t0)/1000000000+"s")
    }
  }
}
