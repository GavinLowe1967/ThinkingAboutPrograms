package thinkingAboutPrograms.factorial

/** An object that calculates factorials. */
object Factorial{
  /** The factorial of n.
    * Precondition: n >= 0. */
  def fact(n: Int): Int = {
    require(n >= 0, "Illegal value for n: "+n)
    var i = 0; var f = 1 // I == f = i! && 0 <= i <= n
    while(i < n){      // I
      i = i+1; f = f*i // I
    }
    // I && i = n so f = n!
    f
  }

  /** Main method. */
  def main(args: Array[String]) = { 
    print("Please input a number: ")
    val n = scala.io.StdIn.readInt()
    if(n >= 0){
      val f = fact(n)
      println("The factorial of "+n+" is "+f)
    }
    else println("Sorry, negative numbers aren't allowed")
  }
}
