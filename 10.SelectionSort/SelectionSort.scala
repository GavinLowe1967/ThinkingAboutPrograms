package thinkingAboutPrograms.selectionSort

/** Object to perform selection sort. */
object SelectionSort{

  /** Sort the array a. */
  def sort(a: Array[Int]) = {
    val n = a.length; var k = 0
    // Invariant: I1 = sorted a[0..k) && a[0..k) <= a[k..n) && 
    //            perm(a, a0) &&0 <= k <= n-1
    while(k < n-1){
      var i = k+1; var m = k
      // Inv: I2 = I1 && a(m) = min a[k..i) && k <= m < i <= n
      while(i < n){
        if(a(i) < a(m)) m = i
        i = i+1
      }
      // I2 && i = n so a(m) = min a[k..n) && k <= m < n
      val t = a(k); a(k) = a(m); a(m) = t // swap a(k) and a(m)
      // sorted a[0..k+1) && a[0..k+1) <= a[k+1..n)
      k = k+1   // I1 reestablished
    }
    // I1 && k = n-1 so sorted a[0..n)
  }

  def main(args: Array[String]) = {
    val n = 20
    val a = Array.fill(n)(scala.util.Random.nextInt(100))
    println(a.mkString(" "))
    sort(a)
    println(a.mkString(" "))
  }
}
