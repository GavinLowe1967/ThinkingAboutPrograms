package thinkingAboutPrograms.heaps

import scala.util.Random

object HeapSortTest{
  /** Number of repetitions. */
  val reps = 100

  /** Do a single test. */
  def doTest = {
    val a = Array.fill(Random.nextInt(20))(Random.nextInt(20))
    val b = a.clone
    // println(a.mkString("<", ",", ">"))
    new HeapSort(a)()
    assert(b.sorted.sameElements(a))
    // println(a.mkString("<", ",", ">"))
  }

  def main(args: Array[String]) = {
    for(i <- 0 until reps) doTest
  }


}
