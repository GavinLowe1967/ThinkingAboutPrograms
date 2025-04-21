package thinkingAboutPrograms.binarySearch

object ArraySearch{

  /** Binary search for x in a. 
    * Pre: a is sorted
    * Post: returns i such that a[0..i) < x ≤ a [i..N) ∧ 0 ≤ i ≤ N. 
    * Note: we provide a as a parameter here, unlike in the book. */
  def binarySearch(a: Array[Int], x: Int): Int = {
    // Invariant I = a[0.. i) < x ≤ a [j..N) ∧ 0 ≤ i ≤ j ≤ N.
    val N = a.length; var i = 0; var j = N
    while(i < j){ // I ∧ i < j.
      val m = (i+j)/2 // i ≤ m < j.
      if(a(m) < x) i = m+1 // I.
      else j = m // I.
    }
    // I ∧ i = j, so a[0..i) < x ≤ a [i..N).
    i
  }
}

import scala.util.Random

/** Object to perform testing on ArraySearch.*/
object ArraySearchTest{
  /** Perform a single random test. */
  def doTest = {
    // Initialise `a` randomly, but non-decreasing.  Each element will be 0, 1
    // or 2 larger than the previous.
    val n = Random.nextInt(1000); val a = new Array[Int](n); var k = 10
    for(i <- 0 until n){ k += Random.nextInt(3); a(i) = k }
    val x = Random.nextInt(2*n+1)
    // Note: x might be smaller than all elements in a, or larger, or
    // in-between.
    val i = ArraySearch.binarySearch(a, x)
    // Check postcondition: enough to look at a(i-1) and a(i). 
    assert(i == 0 || a(i-1) < x); assert(i == n || x <= a(i))
  }

  def main(args: Array[String]) = {
    for(r <- 0 until 1000000){ doTest; if(r%10000 == 0) print(".") }
    println()
  }
}
