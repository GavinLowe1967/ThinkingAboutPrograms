package thinkingAboutPrograms.bitMaps

/** A bitmap, representing a subset of {0..N-1}. */
class BitMapSet(val N: Int) extends Set{
  private val length = (N-1)/64+1 // ceiling of N/64
  private val a = new Array[Long](length)
  private def entryFor(e: Int) = e >> 6 // = e/64
  private def bitFor(e: Int) = e & 63 // = e%64
  private def maskFor(e: Int): Long = 1L << bitFor(e) 

  /* e is in the set iff bit bitFor(e) of a(entryFor(e)) = 1
   *  Abs: S = { e | 0 <= e < N && (a(entryFor(e)) & maskFor(e)) != 0 }. */
  private var theSize = 0 // the number of elements in the set
  // datatype invariant: theSize = #S

  /** Check e is in the desired range [0..N). */
  // private def checkRange(e: Int) =  
  //   require(0 <= e && e < N, "Argument not in expected range: "+e)
  
  /** Add e to the set. */
  def add(e: Int) = { 
    checkRange(e)
    val i = entryFor(e); val mask = maskFor(e)
    if((a(i) & mask) == 0){ a(i) |= mask; theSize += 1; true }
    else false
  }

  /** Test if e is in the set. */
  def contains(e: Int): Boolean = {
    checkRange(e)
    val i = entryFor(e); val mask = maskFor(e)
    (a(i) & mask) != 0
  }

  /** Remove e from the set. */
  def remove(e: Int): Boolean = { 
    checkRange(e)
    val i = entryFor(e); val mask = maskFor(e)
    if((a(i) & mask) != 0){ a(i) ^= mask; theSize -= 1; true}
    else false 
  }

  /** Return the size of the set. */
  def size: Int = theSize

  /** Are this and that equal? */
  override def equals(that: Any): Boolean = that match {
    case s: BitMapSet => 
      if(N != s.N) false
      else{
        var i = 0
        // Invariant: a[0..i) == s.a[0..i)
        while(i < length && a(i) == s.a(i)) i += 1
        i == length
      }
    case _ => false
  }

  /** Convert to a string. */
  override def toString: String = 
    (for(e <- 0 until N; if contains(e)) yield e).mkString("{", ", ", "}")
}


/** Companion object. */
object BitMapSet{
  /** Construct a new BitMapSet over {0..N-1}, containing xs. */
  def apply(N: Int)(xs: Int*): BitMapSet = {
    val s = new BitMapSet(N)
    for(x <- xs) s.add(x)
    s
  }
}
