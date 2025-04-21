package thinkingAboutPrograms.bitMaps

/** A bitmap, representing a subset of {0..N-1}. */
class BitMapSet0(val N: Int) extends Set{
  private val a = new Array[Boolean](N)
  // a(i) = true iff i is in the set; i.e. this represents the set
  // { i | 0 <= i < N && a(i) }
  private var theSize = 0 // the number of elements in the set
  // datatype invariant: size = #{ i | 0 <= i < N && a(i) }
  
  /** Add e to the set. */
  def add(e: Int): Boolean = { 
    checkRange(e)
    if(!a(e)){ theSize += 1; a(e) = true; true }
    else false
  }

  /** Test if e is in the set. */
  def contains(e: Int): Boolean = { checkRange(e); a(e) }

  /** Remove e from the set. */
  def remove(e: Int): Boolean = { 
    checkRange(e)
    if(a(e)){ theSize -= 1; a(e) = false; true }
    else false
  }

  /** Return the size of the set. */
  def size: Int = theSize

  /** Test if this and that are equal. */
  override def equals(that: Any): Boolean = that match {
    case s: BitMapSet0 => 
      if(N != s.N) false
      else{
        var i = 0
        // Invariant: a[0..i) == s.a[0..i)
        while(i < N && a(i) == s.a(i)) i += 1
        i == N
      }
    case _ => false
  }

  /** Convert to a string. */
  override def toString: String = 
    (for(i <- 0 until N; if a(i)) yield i).mkString("{", ", ", "}")
}


/** Companion object. */
object BitMapSet0{
  /** Construct a new BitMapSet over {0..N-1}, containing xs. */
  def apply(N: Int)(xs: Int*): BitMapSet0 = {
    val s = new BitMapSet0(N)
    for(x <- xs) s.add(x)
    s
  }
}
