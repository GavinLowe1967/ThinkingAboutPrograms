package thinkingAboutPrograms.bitMaps

/** A set from the range [0..N). */
trait Set{
  val N: Int
  /* State S : P Int.
   * Init S = {}. 
   * DTI S subset of [0..N). */

  /** Check e is in the desired range [0..N). */
  protected def checkRange(e: Int) = 
    require(0 <= e && e < N, "Argument not in expected range: "+e)

  /** Add x to the set.
    * Pre 0 <=  x < N.
    * Post S = S_0 U {x} && returns (x not in S_0). */
  def add(x: Int): Boolean

  /** Test whether the set contains x. 
    * Pre 0 <= x < N.
    * Post S = S_0 && returns (x in S). */
  def contains(x: Int): Boolean

  /** Remove x from the set.
    * Pre 0 <= x < N.
    * Post S = S_0 - {x} && returns (x in S_0). */
  def remove(x: Int): Boolean
} 
