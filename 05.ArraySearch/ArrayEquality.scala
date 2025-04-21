package thinkingAboutPrograms.arraySearch

/** Object containing function to compare two arrays for value equality. */
object ArrayEquality{
  /** Do `a` and `b` contain the same elements? */
  def sameElements[A](a: Array[A], b: Array[A]): Boolean = {
    if(a.length != b.length) false
    else{
      var k = 0; val n = a.length
      // Invariant: a[0..k) = b[0..k) && 0 \le k \le n
      while(k < n && a(k) == b(k)) k += 1
      k == n
    }
  }

  def main(args: Array[String]) = {
    assert(args.size == 2)
    val a = args(0).toArray; val b = args(1).toArray
    println(sameElements(a, b))
  }

}
