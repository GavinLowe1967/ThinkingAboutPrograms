package thinkingAboutPrograms.wordPaths

import thinkingAboutPrograms.dictionary.Dictionary

/** Program to find a path of words from a source word to a target word,
  * changing one letter at a time.  This can be used either with the
  * breadth-first search algorithm described in the book, or with the A*
  * search algorithm (via the "--astar" switch). */
object WordPaths{
  private var dict: Dictionary = null

  /** Find all neighbours of w */
  protected def neighbours(w: String) : List[String] = {
    var result = List[String]()
    for(i <- 0 until w.length; c <- 'a' to 'z'; if c != w(i)){
      val w1 = w.patch(i, List(c), 1) // replace ith character of w with c
      if(dict.isWord(w1)) result = w1 :: result
    }
    result 
  }

  /** A type representing paths through the graph of words */
  type Path = List[String]
  
  /** Print the Path path, separating entries with commas */
  def printPath(path: Path) = println(path.mkString(", "))
  // OR: print(path.head); for(w <- path.tail) print(", "+w); println

  /** Find a minimum length path from start to target 
    * @return Some[path] for some shortest Path path if one exists; otherwise
    *         None. */
  private def findPath(start: String, target: String): Option[Path] = {
    // We'll perform a breadth-first search.  Each node of the search graph
    // will be a list of words, consecutive words differing in one letter, and
    // ending with start, thereby representing a path (in reverse order)
    val queue = scala.collection.mutable.Queue(List(start))
    // Keep track of the words we've already considered
    val seen = new scala.collection.mutable.HashSet[String]
    seen += start

    while(!queue.isEmpty){
      val path = queue.dequeue(); val w = path.head
      for(w1 <- neighbours(w)){
	if(w1==target) return Some((target::path).reverse)
	else if(!seen.contains(w1)){ seen += w1; queue += w1::path }
      } // end of for
    } // end of while
    None // no solutions found
  } // end of findPath

  def main(args:Array[String]) = {
    // parse arguments
    var i = 0; var dictFile = "knuth_words"; var astar = false
    while(i < args.length) args(i) match{
      case "-d" => dictFile = args(i+1); i += 2
      case "--astar" => astar = true; i += 1
      case arg => println("Illegal argument: "+arg); sys.exit()
    }

    // initialise dictionary 
    dict = new Dictionary(dictFile)

    // The main stuff
    var done = false
    while(!done){
      val start = scala.io.StdIn.readLine("Start word (blank to finish)? ")
      if(start == "") done = true
      else{
        val target = scala.io.StdIn.readLine("Target word? ")
        val t0 = System.currentTimeMillis()
        val optPath = findPath(start, target)
        optPath match{
          case Some(path) => printPath(path)
          case None => println("No path found")
        }
        println("Time taken: "+(System.currentTimeMillis()-t0)+"ms")
      }
    }    
  }
}
