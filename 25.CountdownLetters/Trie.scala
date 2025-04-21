package thinkingAboutPrograms.countdown

/** A trie, implementing a Dictionay. */ 
class Trie{
  /** Array storing the next nodes in the Trie. */
  protected val succs = new Array[Trie](26)

  /** Does this Trie node correspond to a complete word? */
  protected var done = false

  /** Does this correspond to a complete word? */
  def complete = done

  /** Convert c to an Int in the range [0..26).
    * Pre: 'a' <= c <= 'z'. */
  protected def charToInt(c: Char): Int = c-'a'

  /** Add st to this Trie. */
  def add(st: String): Unit =
    if(st.isEmpty) done = true
    else{
      val c = charToInt(st.head)
      assert(0 <= c && c < 26, st)
      if(succs(c) == null) succs(c) = new Trie
      succs(c).add(st.tail)
    }	

  /** Does this Trie contain st? */ 
  def contains(st: String): Boolean =
    if(st.isEmpty) done
    else{
      val c = charToInt(st.head)
      succs(c) != null && succs(c).contains(st.tail)
    }

  /** Get the Trie reached by following character c. */
  def follow(c: Char): Trie = succs(charToInt(c))

  /** Is this trie empty, i.e. containing no words? */
  private def isEmpty = {
    if(done) false
    else{
      var i = 0
      while(i < 26 && succs(i) == null) i += 1
      i == 26
    }
  }

  /** Remove st from this Trie.  Exercise. */
  def remove(st: String): Boolean = {
    if(st.isEmpty){
      val result = done; done = false; result
    }
    else{
      val c = charToInt(st.head)
      if(succs(c) == null) false
      else{ 
        val result = succs(c).remove(st.tail)
        // Remove succs(c) if now empty.
        if(succs(c).isEmpty) succs(c) = null
        result
      }
    }
  }
}

import thinkingAboutPrograms.util.MyTest._

/** A simple test program. */
object TrieTest{
  def main(args: Array[String]) = {
    val tr = new Trie; val words = List("cat", "can", "dog", "do")
    for(word <- words) tr.add(word)
    test("contains"){ words.forall(tr.contains(_)) }
    test("non contains"){ ! tr.contains("cow") && ! tr.contains("ca") }
    test("non remove"){ ! tr.remove("cow") }
    test("remove"){ tr.remove("cat") }
    test("removed"){ !tr.contains("cat") }
  }
}

