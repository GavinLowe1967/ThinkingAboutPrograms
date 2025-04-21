package thinkingAboutPrograms.exponentiation

/** An efficient iterative definition of exponentiation. */
object FastExp{
  /** Calculate x^n
    * Precondition: n >= 0. */
  def exp(x:Double, n:Long) : Double = {
    require(n >= 0)
    // Invariant I: y * z^k = x^n && 0 <= k <= n
    // Variant: k
    var y = 1.0; var z = x; var k = n // I
    while(k>0){
      // I
      if(k%2==0){         // I
	z = z*z; k = k/2  // I
      }
      else{    	                  // I
	y = y*z; z = z*z; k = k/2 // I 
      }
    }
    // I && k=0, so y=x^n
    y
  }

  def main(args:Array[String]) = {
    if(args.length != 2) println("Usage: scala FastExp x n");
    else{
      val x = args(0).toDouble; val n = args(1).toLong
      val t0 = java.lang.System.nanoTime
      if(n>=0) println(x.toString+"^"+n+" = "+exp(x,n))
      else println("Error: second argument should be non-negative")
      println("Time taken: "+(java.lang.System.nanoTime-t0)/1000000+"ms")

    }
  }
}
