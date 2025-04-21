package thinkingAboutPrograms.dictionary

/** Each object implementng this trait represents a dictionary, in which words
  * can be looked up. */
trait DictionaryT{
  /** Test if w is in the dictionary. */
  def isWord(w: String): Boolean
}

// -------------------------------------------------------

/** Each object of this class represents a dictionary, in which words can 
  * be looked up.
  * @param fname the name of a file containing a suitable list of words, one 
  * per line. */
class Dictionary(fname: String) extends DictionaryT{
  /** A Set object holding the words. */
  private val words = new scala.collection.mutable.HashSet[String]

  /** Initialise dictionary from fname. */
  private def initDict(fname: String) = {
    val source = scala.io.Source.fromFile(fname)
    val allWords = source.getLines()
    for(w <- allWords) words += w
  }

  // Initialise the dictionary
  initDict(fname)

  /** Test if w is in the dictionary */
  def isWord(w: String): Boolean = words.contains(w)
}

