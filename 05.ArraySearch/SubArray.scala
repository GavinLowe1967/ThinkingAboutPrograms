package thinkingAboutPrograms.arraySearch

/** Testing whether one array is included in another. */
object SubArray{
  /** Does a contain the elements of b consecutively? */
  def includes[A](a: Array[A], b: Array[A]): Boolean = {
    val aLen = a.length; val bLen = b.length
    var k = 0; var found = false
    // Inv: found iff a[i..i+bLen) = b[0..bLen) for some i in [0..k)
    while(k <= aLen-bLen && !found){
      // set found = true if a[k..k+bLen) = b[0..bLen)
      // Invariant: a[k..k+j) = b[0..j) && 0 <= j <= bLen
      var j = 0
      while(j < bLen && a(k+j) == b(j)) j += 1
      found = j == bLen
      k += 1
    }
    found
  }

  def main(args: Array[String]) = {
    assert(args.length == 2)
    println(includes(args(0).toArray, args(1).toArray))
  }

}
