package thinkingAboutPrograms.sudoku

/** A bit map of size 9*9*9. */
class LegalBitMap(bits: Array[Long]){
  /* Each bit has an index in the range [0..9*9*9). */

  /** The index for (i,j,d), in the range [0..9*9*9). */
  @inline private def indexFor(i: Int, j: Int, d: Int) = {
    require(0 <= i && i < 9 && 0 <= j && j < 9 && 1 <= d && d <= 9, s"$i $j $d")
    (i*9+j)*9+(d-1)
  }

  /** The entry in bits holding the entry with index ix. */
  @inline private def entryFor(ix: Int) = ix >> 6

  /** Bit mask to extract the entry with index ix. */
  @inline private def maskFor(ix: Int): Long = 1L << (ix & 63)

  /** The entry for (i,j,d). */
  def apply(i: Int, j: Int, d: Int): Boolean = {
    val ix = indexFor(i, j, d)
    (bits(entryFor(ix)) & maskFor(ix)) != 0
  }

  /** Update the entry for (i,j,d) with b. */
  def update(i: Int, j: Int, d: Int, b: Boolean) = {
    val ix = indexFor(i,j,d); val entry = entryFor(ix); val mask = maskFor(ix)
    require(entry < LegalBitMap.Size, s"$i $j $d")
    if(b) bits(entry) |= mask 
    else{
      // println(s"$i $j $d $ix $entry $mask ${bits(entry)}")
      if((bits(entry) & mask) != 0) bits(entry) ^= mask
      assert(!apply(i,j,d), s"$i $j $d $ix $entry $mask ${bits(entry)}") 
    }
  }

  /** Clone this. */
  override def clone = new LegalBitMap(bits.clone)
}

// =======================================================

object LegalBitMap{
  private val Size = 12 // ceiling(9*9*9 / 64)

  /** Factory method.  Initially all bits should be 1. */
  def apply() = new LegalBitMap(Array.fill(Size)((-1).asInstanceOf[Long]))
}
