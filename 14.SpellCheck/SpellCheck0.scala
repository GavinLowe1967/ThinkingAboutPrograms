package thinkingAboutPrograms.spellCheck

/** A simple application to check the spelling of words */
object SpellCheck0{
  /** The dictionary (arguably, this should be in a separate module) */
  val words = new scala.collection.mutable.HashSet[String] 

  /** Initialise dictionary from fname */
  def initDict(fname: String) = {
    val source = scala.io.Source.fromFile(fname)
    val allWords = source.getLines()
    for(w <- allWords) words += w
  }

  /** test if w is in the dictionary */
  def isWord(w: String) : Boolean = words.contains(w)

  def main(args: Array[String]) = {
    if(args.length != 1) println("Please provide one argument")
    else{
      val w = args(0)
      initDict("knuth_words")
      if(isWord(w)) println(w+" is a valid word")
      else println("word "+w+" not found")
    }
  }
}
