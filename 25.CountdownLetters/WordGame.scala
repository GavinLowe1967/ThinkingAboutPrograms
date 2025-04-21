package thinkingAboutPrograms.countdown


class WordGame(fname: String = "knuth_words"){
  /** The Trie used in the search. */ 
  private val trie = new Trie

  /** Is c a lower case character? */
  private def isLower(c: Char) = 'a' <= c && c <= 'z'

  /* Initialise the Trie based upon file fname. */
  private val source = scala.io.Source.fromFile(fname)
  for(w <- source.getLines()){
    if(w.forall(isLower)) trie.add(w)
  }

  /** The result of removing c from st.  Pre: st contains c. */
  private def remove(st: List[Char], c: Char): List[Char] = 
    if(st.head == c) st.tail else st.head :: remove(st.tail, c)

  /** Search for the longest word that can be formed from st in t. */
  private def searchRec(st: List[Char], t: Trie): Option[String] = {
    var longest = if(t.complete) Some("") else None
    var longestLength = 0
    for(c <- st.distinct){
      val t1 = t.follow(c)
      if(t1 != null) searchRec(remove(st, c), t1) match{
        case Some(w) if w.length >= longestLength =>
          longest = Some(c+:w); longestLength = w.length+1
        case _ => { }
      }
    }
    longest
  }

  /** Search for the longest word that can be formed from st. */
  def search(st: String): Option[String] = searchRec(st.toList, trie)
}

/** A front end to the WordGame object.  The program either takes a string of
  * letters on the command line, or randomly generates those letters.  It then
  * finds the longest word from those letters. */
object PlayWordGame{
  /* Strings given the distribution of letters to use. */
  private val vowels = "a"*9+"e"*12+"i"*9+"o"*8+"u"*4
  private val consonants =
    "bbcc"+"d"*4+"ffggghhjk"+"l"*4+"mm"+"n"*6+"ppq"+"r"*6+"s"*4+"t"*6+"vvwwxyyz"
  
  import scala.util.Random

  /** Create a random collection of 4 vowels and 5 consonants. */
  val randomLetters: String = {
    val numVowels = 3+Random.nextInt(3) // between 3 and 5 vowels
    var st = List[Char](); val v = vowels.length; val c = consonants.length
    for(_ <- 0 until numVowels) st ::= vowels(Random.nextInt(v))
    for(_ <- 0 until 9-numVowels) st ::= consonants(Random.nextInt(c))
    new String(st.toArray.sorted) // .toString
  }

  def main(args: Array[String]) = {
    var letters = if(args.nonEmpty) args(0) else randomLetters
    println(letters)
    val wg = new WordGame
    // Thread.sleep(30000)
    wg.search(letters) match{
      case Some(w) => println(w)
      case None => println("No word possible!")
    }
  }
}
