package thinkingAboutPrograms.sudoku

import scala.collection.mutable.ArrayBuffer

import Partial._

/** A solver for sudoku puzzles, based on either SimplePartial or
  * AdvancedPartial. */
object Sudoku{
  /** Solve the puzzle described by init.
    * @param all should all solutions be found? */
  def solve(init: Partial, all: Boolean, avoidCloning: Boolean = false): Unit = {
    val stack = new scala.collection.mutable.Stack[Partial]
    stack.push(init)
    var states = 0 // number of states expanded

    while(!stack.isEmpty){
      val partial = stack.pop()
      states += 1
      if(partial.complete){  // done!
	partial.printPartial
	println("Explored "+states+" states")
        if(!all) return
      }
      else if(avoidCloning){
        val ((i,j), cost) = partial.nextPosWithCost
        var count = 0
	for(d <- digits; if partial.canPlay(i,j,d)){
          count += 1
	  if(count < cost){ val p1 = partial.play(i,j,d); stack.push(p1) }
          else{ partial.playHere(i,j,d); stack.push(partial) }
	}
        assert(count == cost)
      }
      else{
	// Choose position to play
	val (i,j) = partial.nextPos
	// Consider all values to play there
	for(d <- digits; if partial.canPlay(i,j,d)){
	  val p1 = partial.play(i,j,d); stack.push(p1)
	}
      }
    } // end of while
    println("Total states explored: "+states)
  }

  def main(args:Array[String]) = {
    val t0 = System.currentTimeMillis()

    // options    
    var all = false // find all solutions
    var adv = false // do we use AdvancedPartial?
    var pQueue = false // do we use PQueueAdvancedPartial
    var useLocMap = false // do we use the specialised location map in the
                          // priority queue?
    var avoidCloning = false
    var count = 1 // number of tests
    var fname = "" // filename
    // parse command line arguments
    var i = 0
    while(i < args.length) args(i) match{
      case "--all" => all = true; i += 1
      case "-a" => adv = true; i += 1
      case "--pQueue" => pQueue = true; i += 1
      case "--locMap" => useLocMap = true; i += 1
      case "--avoidCloning" => avoidCloning = true; i += 1
      case "-n" => count = args(i+1).toInt; i += 2
      case f => assert(fname.isEmpty); fname = f; i += 1
    }
    if(fname == ""){ println("No input file specified."); sys.exit() }

    // Initialise partial from file
    val partial: Partial =
      if(pQueue) new PQueueAdvancedPartial(useLocMap)
      else if(adv) new AdvancedPartial()
      else new SimplePartial
    partial.init(fname)
    // And solve
    solve(partial, all, avoidCloning)
    println("Time taken: "+(System.currentTimeMillis()-t0))
  }    
}

// With SimplePartial, it takes about 140ms to find a solution; with
// AdvancedPartial, it takes about 3ms (averaged over 1000 solutions).
    
