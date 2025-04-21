package thinkingAboutPrograms.quicksort

object Quicksort{
  private var a: Array[Int] = null

  /** Partition the segment a[l..r) . */
  private def partition(l: Int, r: Int) : Int = {
    val x = a(l) // The value of the pivot.
    var i = l+1; var j = r
    while(i < j){
      if(a(i) < x) i += 1
      else{ val t = a(i); a(i) = a(j-1); a(j-1) = t; j -= 1 }
    }
    a(l) = a(i-1); a(i-1) = x // Swap the pivot into position.
      i-1 // The index of the pivot.
  }

  /** Sort a, in situ.
    * post : sorted a ∧ perm(a, a_0). */
  def QSort(): Unit = QSort(0, a.length)

  /** Sort a. */
  def QSort(a: Array[Int]): Unit = { this.a = a; QSort() }

  /** Sort the segment a[l..r) , in situ. */
  private def QSort(l: Int, r: Int) : Unit =
    if(r-l > 1){ // Nothing to do if segment is empty or singleton.
      val k = partition(l,r)
      QSort(l,k); QSort(k+1,r)
    }

  def main(args: Array[String]) = {
    val n = 20; a = Array.fill(n)(scala.util.Random.nextInt(100))
    println(a.mkString(" "))
    QSort()
    println(a.mkString(" "))
  }
}
