package thinkingAboutPrograms.arraySearch

/** Various functions for searching in an array.  
  * 
  * Usage: scala ArraySearch alg a x
  * where alg is an integer between 1 and 5, indicating the algorithm to use, 
  * and a is a string to search, and x is a single character to search for. */
object ArraySearch{
  /** Does a contain x? */
  def contains1[A](a: Array[A], x: A): Boolean = {
    var k = 0; var found = false; val n = a.length
    // Invariant: (found <=> x in a[0..k)) && 0 <= k <= n
    while(k < n){
      found = found || a(k) == x
      k += 1
    }
    found
  }

  /** Does a contain x? */
  def contains2[A](a: Array[A], x: A): Boolean = {
    var k = 0; var found = false; val n = a.length
    // Invariant: (found <=> x in a[0..k)) && 0 <= k <= n
    while(k < n && !found){
      found = found || a(k) == x
      k += 1
    }
    found
  }

  /** Does a contain x? */
  def contains3[A](a: Array[A], x: A): Boolean = {
    var k = 0; var found = false; val n = a.length
    // Invariant: (found <=> x in a[0..k)) && 0 <= k <= n
    while(k < n && !found){
      found = a(k) == x
      k += 1
    }
    found
  }

  /** Does a contain x? */
  def contains4[A](a: Array[A], x: A): Boolean = {
    var k = 0; val n = a.length
    // Invariant: x not in a[0..k) && 0 <= k <= n
    while(k < n && a(k) != x) k += 1
    k < n
  }

  /** Does a contain x? */
  def contains5[A](a: Array[A], x: A): Boolean = {
    var k = 0; val n = a.length
    // Invariant: x not in a[0..k) && 0 <= k <= n
    while(k < n)
      if(a(k) == x) return true else k += 1
    false
  }

  def main(args: Array[String]) = {
    assert(args.size == 3);
    val alg = args(0); val a = args(1).toArray; 
    assert(args(2).length == 1); val x = args(2).head
    alg match{
      case "1" => println(contains1(a,x))
      case "2" => println(contains2(a,x))
      case "3" => println(contains3(a,x))
      case "4" => println(contains4(a,x))
      case "5" => println(contains5(a,x))
    }
  }
}
