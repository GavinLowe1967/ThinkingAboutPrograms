package thinkingAboutPrograms.bitMaps

import scala.util.Random

object BitMapSetTest{
  val iters = 10000 // # operations per run
  val reps = 50000  // # runs

  /** Do a single test. */
  def doTest = {
    val N = Random.nextInt(200)+1 // size of the set
    val bms = new BitMapSet(N)
    // val bms = new BitMapSet0(N)
    val s = new scala.collection.mutable.HashSet[Int]
    // We test bms against s

    for(i <- 0 until iters){
      val choice = Random.nextInt(3)
      val n = Random.nextInt(N)
      if(choice == 0) assert(s.add(n) == bms.add(n), "add "+n) 
      else if(choice == 1) assert(s.remove(n) == bms.remove(n), "remove "+n)
      else assert(s.contains(n) == bms.contains(n), "contains "+n)
    }
  }

  def main(args: Array[String]) = {
    for(r <- 0 until reps){
      doTest
      if(r%500 == 0) print(".")
    }
    println()
  }
}
  
