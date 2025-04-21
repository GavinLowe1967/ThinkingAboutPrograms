package thinkingAboutPrograms.maxSegSum

/** Object including the maximum segment sum programs from Chapter 13. */
object MaxSegSum{
  /** The sum of a[p..q). */
  def segsum(a: Array[Int], p: Int, q: Int) : Int = {
    var sum = 0
    for(i <- p until q) sum += a(i)
    sum
  }

  /** The algorithm from Section 13.1. */
  def mss1(a: Array[Int]): Int = {
    var mss = 0; var n = 0; val N = a.size
    while(n < N){
      n += 1; var m = 0
      while(m <= n){ mss = mss max segsum(a,m,n); m += 1 }
    }
    mss
  }

  /** The algorithm from Section 13.2. */
  def mss2(a: Array[Int]): Int = {
    var mss = 0; var n = 0; val N = a.size
    while(n < N){
      n += 1; var m = n; var ss = 0
      while(m > 0){ m -= 1; ss += a(m); mss = mss max ss }
    }
    mss
  }

  /** The algorithm from Section 13.3. */
  def mss3(a: Array[Int]): Int = {
    var n = 0; var mss = 0; var mrss = 0; val N = a.size
    while(n < N){
      n = n+1
      mrss = 0 max (mrss + a(n-1))
      mss = mss max mrss 
    }
    mss
  }
}


// =======================================================

import scala.util.Random

/** Tests of the above code. */
object MaxSegSumTest{
  /** Do a single test. */
  def doTest = {
    // Generate array of n random numbers in range [-50..50). 
    val n = Random.nextInt(100); val a = Array.fill(n)(Random.nextInt(100)-50)
    // Check all three algorithms give the same result. 
    val s1 = MaxSegSum.mss1(a); val s2 = MaxSegSum.mss2(a)
    val s3 = MaxSegSum.mss3(a)
    assert(s2 == s1, a.mkString("(",",",")")+s" $s1; $s2")
    assert(s3 == s1, a.mkString("(",",",")")+s" $s1; $s3")
  }

  def main(args: Array[String]) = {
    for(r <- 0 until 20000){ doTest; if(r%1000 == 0) print(".") }
    println()
  }


}
