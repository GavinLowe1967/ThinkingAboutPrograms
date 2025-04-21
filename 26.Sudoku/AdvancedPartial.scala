package thinkingAboutPrograms.sudoku 

import Partial._

/** The following abstract class contains code common to AdvancedPartial and
  * PQueueAdvancedPartial.  It provides most of the functionality of PartialT.
  * Implementing classes must provide definitions for nextPos and play. */
abstract class AdvancedPartialT(legal: LegalBitMap) extends BasicPartial{

  // DTI: if contents(i)(j) = 0 then
  // legal(i)(j)(d) <=>  board (+) {(i,j) -> d} satisfies the abstract DTI

  /** Can we play value d in position (i,j); precondition: (i,j) is blank. */
  def canPlay(i: Int, j: Int, d: Int) : Boolean = legal(i,j,d)

  /** Initialise from a file. */
  override def init(fname:String) = {
    super.init(fname)
    // Note: legal should be all true at this point.
    // Now update legal lists based on values played. 
    for(i <- rows; j <- columns){
      val d = contents(i)(j); if(d != 0) updateLegal(i, j, d)
    }
  }

  /** Update legal lists corresponding to d being played in position (i,j):
    * remove d from the rest of this row, column and block.  */
  protected def updateLegal(i: Int, j: Int, d: Int) = {
    for(d1 <- digits) legal.update(i,j,d1,false)
    for((i1,j1) <- neighbours(i)(j)) legal.update(i1,j1,d,false)
  }

  /** The cost of playing in (i,j): the number of digits that can be played
    * there legally.  In range [0..9]. */
  protected def cost(i: Int, j: Int) : Int = {
    assert(contents(i)(j) == 0)
    var c = 0; var d = 1
    while(d <= 9){ if(legal(i,j,d)) c += 1; d += 1 }
    c
  }

  def printLegal = {
    for(i <- rows){
      for(d <- digits){
        for(j <- columns) print(if(legal(i,j,d)) "T" else "F")
        print(" ")
      }
      println()
    }
  }
}

// =======================================================

/** A more efficient implementaiton of partial solutions, which aims to pick a
  * square with the fewest legal moves.
  * @param legal a bitmap such that legal(i)(j)(d) shows whether it is legal to 
  * play digit d in position (i,j).  */
class AdvancedPartial(legal: LegalBitMap) extends AdvancedPartialT(legal){
  /** Auxiliary constructor with a new LegalBitMap. */ 
  def this() = this(LegalBitMap())

  /** Find a blank position to play in; precondition: complete returns false.
    * We find the square that gives the best score, as defined below. */
  def nextPos: Coord = {
    var bestCost = 10; var bestPos = (0,0)
    for(i <- rows; j <- columns; if contents(i)(j) == 0){
      val thisCost = cost(i,j)
      if(thisCost < bestCost){ bestPos = (i,j); bestCost = thisCost }
    }
    assert(bestCost < 10); bestPos
  }

  /** Find a blank position, and the number of options for playing there. */
  def nextPosWithCost: (Coord, Int) = ???

  /** Extend this partial solution by playing d in position (i,j). */
  def playHere(i: Int, j: Int, d: Int) = {
    contents(i)(j) = d; updateLegal(i,j,d)
  }

  /** Create a new partial solution, extending this one by playing d in
    * position (i,j). */
  def play(i: Int, j: Int, d: Int) : Partial = {
    val p = new AdvancedPartial(legal.clone); copyContents(p)
    // Now play d in (i,j) of p.
    p.playHere(i, j, d); p
  }
}

// =======================================================

/** An implementation of Partial, using a priority queue to store coordinates
  * where we might play next. 
  * @param legal a mapping mapping  tuples (i,j,d) to a Boolean indicating
  * whether it is legal to play d at (i,j).
  * @param pQueue a priority queue of coordinates where we could try playing 
  * each digit. */
class PQueueAdvancedPartial(legal: LegalBitMap, pQueue: PriorityQueue)
    extends AdvancedPartialT(legal){

  /** Constructor for starting. 
    * @param useLocMap do we use the specialised location map. */
  def this(useLocMap: Boolean) = 
    this(LegalBitMap(), 
      if(useLocMap) new LocMapPriorityQueue(81) else new HashMapPriorityQueue(81)
    )

  /* We maintain the DTI
   *  (i,j) in pQueue <=> contents(i)(j) = 0. */

  /** Initialise from a file. */
  override def init(fname:String) = {
    super.init(fname)
    for(i <- rows; j <- columns; if contents(i)(j) == 0)
      pQueue.add((i,j), cost(i,j))
  }

  def nextPos: Coord = pQueue.get()

  def nextPosWithCost: (Coord, Int) = pQueue.getWithCost()

  def play(i: Int, j: Int, d: Int): PQueueAdvancedPartial = {
    val p = new PQueueAdvancedPartial(legal.clone, pQueue.clone)
    copyContents(p); p.playHere(i,j,d); p
  }

  /** Add d at (i,j) in this. */
  def playHere(i: Int, j: Int, d: Int) = {
    contents(i)(j) = d 
    // Not legal to play in (i,j)
    for(d1 <- digits) legal.update(i,j,d,false)
    // Update entries in legal and pQueue based on this play.
    for((i1,j1) <- neighbours(i)(j)){
      if(contents(i1)(j1) == 0 && legal(i1,j1,d)){ 
        legal.update(i1,j1,d,false)
        // This reduces the cost for (i1,j1) by 1.
        pQueue.decrementCost((i1,j1))
      }
    }
  }
}
