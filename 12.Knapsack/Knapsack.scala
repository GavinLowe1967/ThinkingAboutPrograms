package thinkingAboutPrograms.dynamicProgramming

import scala.util.Random
import java.lang.System.nanoTime

/** An object to solve the knapsack problem, given the values and weights of
  * objects. */
class Knapsack(value: Array[Int], weight: Array[Int]){
  val N = value.length; require(weight.length == N)

  /** Optimal solution from objects [0..n) and max weight w.
    * This version uses the recursive algorithm from Section 12.1, 
    * without memoisation. */
  def solveRec(n: Int, w: Int): Int = {
    if(n == 0) 0
    else if(weight(n-1) > w) solveRec(n-1, w)
    else solveRec(n-1, w) max (solveRec(n-1, w-weight(n-1)) + value(n-1))
  }

  /** Find the optimal solution.  This version uses memoisation. 
    * @return the value of the optimal solution, and an array of booleans
    * indicating which objects to include.
    *  
    * This version uses a while loop.*/
  def solveX(W: Int): (Int, Array[Boolean]) = {
    // Solutions found so far: solns(n)(w) gives the best value obtainable for
    // objects [0..n) and weight w.
    val solns = Array.ofDim[Int](N+1, W+1)
    // include(n)(w) indicates whether object n-1 is included in the optimal
    // solution for objects [0..n) and weight w.
    val include = Array.ofDim[Boolean](N+1, W+1)

    // Store optimal solution for objects [0..n) and maximum weight w in
    // solns(n)(w).
    for(w <- 0 to W) solns(0)(w) = 0
    var n = 1
    while(n <= N){
      var w = 0
      while(w <= W){
        // Consider inserting object n-1
        if(weight(n-1) > w) solns(n)(w) = solns(n-1)(w)
        else{
          // The maximum values obtainable if object n-1 is or is not
          // included, respectively
          val includeValue = solns(n-1)(w-weight(n-1)) + value(n-1)
          val notIncludeValue = solns(n-1)(w)
          if(includeValue > notIncludeValue){ 
            solns(n)(w) = includeValue; include(n)(w) = true 
          }
          else solns(n)(w) = notIncludeValue
        }
        w += 1
      } // end of while(w <= W)
      n += 1
    }
    // Reproduce the optimal packing
    var packing = new Array[Boolean](N); var w = W
    for(n <- N to 1 by -1; if include(n)(w)){
      packing(n-1) = true; w -= weight(n-1) 
    }

    (solns(N)(W), packing)
  }

  /** Find the optimal solution.  This version uses memoisation. 
    * @return the value of the optimal solution, and an array of booleans
    * indicating which objects to include.
    * 
    * This version is the version from Section 12.3.  It uses for loops.  */
  def solve(W: Int): (Int, Array[Boolean]) = {
    // Solutions found so far: solns(n)(w) gives the best value obtainable for
    // objects [0..n) and weight w.
    val solns = Array.ofDim[Int](N+1, W+1)
    // include(n)(w) indicates whether object n-1 is included in the optimal
    // solution for objects [0..n) and weight w.
    val include = Array.ofDim[Boolean](N+1, W+1)

    // Store optimal solution for objects [0..n) and maximum weight w in
    // solns(n)(w).
    for(w <- 0 to W) solns(0)(w) = 0
    for(n <- 1 to N; w <- 0 to W){
      // Consider inserting object n-1
      if(weight(n-1) > w) solns(n)(w) = solns(n-1)(w)
      else{
        // The maximum values obtainable if object n-1 is or is not
        // included, respectively
        val includeValue = solns(n-1)(w-weight(n-1)) + value(n-1)
        val notIncludeValue = solns(n-1)(w)
        if(includeValue > notIncludeValue){
          solns(n)(w) = includeValue; include(n)(w) = true
        }
        else solns(n)(w) = notIncludeValue
      }
    }
    // Reproduce the optimal packing
    var packing = new Array[Boolean](N); var w = W
    for(n <- N to 1 by -1; if include(n)(w)){
      packing(n-1) = true; w -= weight(n-1) 
    }

    (solns(N)(W), packing)
  }

  /** A recursive version with memorisation; this is slower than solve. */
  def solveBack(N: Int, W: Int): Int = {
    val solutions = Array.fill[Int](N+1, W+1)(-1)
    // Solve for i and w, recording result
    def rec(i: Int, w: Int): Int = {
      val s = solutions(i)(w)
      if(s >= 0) s
      else{
        val s1 =
          if(i == 0) 0
          else if(weight(i-1) > w) rec(i-1, w)
          else rec(i-1, w) max rec(i-1, w-weight(i-1)) + value(i-1)
        solutions(i)(w) = s1
        s1
      }
    }

    rec(N, W)
  }
}

// -------------------------------------------------------

/** Object to run experiments on the solvers. */
object KnapsackTest{
  val Million = 1000000

  /** Perform the computation comp, and return the time taken. */
  def time[A](comp: => A): A = {
    val t0 = nanoTime
    val res = comp
    println("Time = "+(nanoTime-t0)/Million+"ms")
    res
  }

  def main(args: Array[String]) = {
    var N = 3000; var W = 10000 // # objects; max weight
    var doRec = false // include recursive solution
    // Parse command line
    var i = 0
    while(i < args.length) args(i) match{
      case "-N" => N = args(i+1).toInt; i += 2
      case "-W" => W = args(i+1).toInt; i += 2
      case "--rec" => doRec = true; i += 1
      case arg => println("Unrecognised argument: "+arg); sys.exit()
    }

    // Initialise values and weights.
    val value = new Array[Int](N); val weight = new Array[Int](N)
    val MW = 10; val MV = 10 // max weight, value per object

    for(i <- 0 until N){
      value(i) = 1+Random.nextInt(MV); weight(i) = 1+Random.nextInt(MW)
    }
    // Create solver object
    val solver = new Knapsack(value, weight)
    // Print out weights and values
    println("Object\tWeight\tValue")
    for(i <- 0 until N) println(s"$i\t${weight(i)}\t${value(i)}")
    println()

    // Recursive solution
    if(doRec){
      time{
        val maxVal = solver.solveRec(N, W)
        println("Maximum value from recursive version: "+maxVal)
      }
      println()
    }

    // Memoising solution
    time{
      val (maxVal, packing) = solver.solve(W)
      println("Maximum value from iterative version: "+maxVal)
      println("Objects to include:")
      for(i <- 0 until N; if packing(i)) print(s"$i\t")
      println()
    }
  }
}
