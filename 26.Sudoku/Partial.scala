package thinkingAboutPrograms.sudoku 

import Partial._

/** The Partial trait, corresponding to partial solutions. */
trait Partial{
  /* State: board : {0..8} x {0..8} -|-> {1..9}
   * DTI: forall (i,j), forall (i',j') in neighbours(i,j)
   *        board(i)(j) != 0 implies board(i')(j') != board(i)(j). */
 
  /** Initialise from a file. */
  def init(fname:String): Unit
    // Pre: fname contains 9 lines, each containing 9 characters 
    //      from {1..9} or ".", and obeying the rules of the DTI
    // Post: writing fname(i,j) for the jth character of line i of fname,
    //       forall i, j <- {0..8} (
    //         forall k <- {1..9} (fname(i,j) = k <=> board(i,j) = k) &&
    //         fname(i,j)="." <=> (i,j) not in dom board )

  /** Print partial solution. */
  def printPartial: Unit
    // Pre: complete
    // Post: board=board_0 &&
    //       prints 9 lines, with line i containing board(i)(0)...board(i)(8)

  /** Is the partial solution complete? */
  def complete : Boolean 
    // Post: board=board_0 && returns dom board = {0..8} x {0..8}

  /** Find a blank position. */
  def nextPos : Coord
    // Pre: dom board != {0..8} x {0..8}
    // Post: board=board_0 && returns (i,j) s.t. (i,j) not in dom board

  /** Find a blank position, and the number of options for playing there. */
  def nextPosWithCost: (Coord, Int) 

  /** Can we play value d in position (i,j). */
  def canPlay(i: Int, j: Int, d: Int): Boolean
    // Pre: i,j <- {0..8} && d <- {1..9}
    // Post: returns true iff board (+) {(i,j) -> d} satisfies DTI
    
  /** Create a new partial solution, extending this one by playing d in
    *  position (i,j). */
  def play(i: Int, j: Int, d: Int): Partial 
    // Pre: i,j <- {0..8} && d <- {1..9} && 
    //      board (+) {(i,j) -> d} satisfies DTI
    // Post: board=board_0 && returns p s.t. p.board = board (+) {(i,j) -> d}

  /** Extend this partial solution by playing d in position (i,j). */
  def playHere(i: Int, j: Int, d: Int): Unit
}

// -------------------------------------------------------

object Partial{
  type Coord = (Int, Int)

  val rows = 0 until 9    // row numbers
  val columns = 0 until 9 // column numbers
  val digits = 1 to 9     // digit values

  /** Number of neighbours of each square: 8 in the same row; 8 in the sam
    * column; and 4 in the same 3-by-3 block, not already counted. */
  val NumNeighbours = 20

  /** Array such that neighbours(i)(j) is all squares in same row, column or 3x3
    * block as (i,j). */
  val neighbours = Array.ofDim[Coord](9, 9, NumNeighbours)

  /* Initialise neighbours. */
  for(i <- 0 until 9; j <- 0 until 9) makeNeighbours(i,j)

  /** Initialise neighbours(i)(j). */
  private def makeNeighbours(i: Int, j: Int) = {
    var ix = 0
    @inline def add(i1: Int, j1: Int) = {
      neighbours(i)(j)(ix) = (i1,j1); ix += 1
    }
    for(j1 <- columns; if j1 != j) add(i,j1)
    for(i1 <- rows; if i1 != i) add(i1, j)
    val basei = i/3*3; val basej = j/3*3
    for(i1 <- basei until basei+3; if i1 != i;
      j1 <- basej until basej+3; if j1 != j) add(i1, j1)
    assert(ix == NumNeighbours)
  }
}

// -------------------------------------------------------

/** A partial implementation of Partial, giving code shared by full
  * implementations. */
abstract class BasicPartial extends Partial{
  /** The contents of each square in the grid, where 0 represents a blank
    * square. */
  protected val contents = Array.ofDim[Int](9,9)
  // Abs: forall i, j 
  //        (contents(i)(j)=0 <=> (i,j) not in dom board) && 
  //        (contents(i)(j)>0 <=> contents(i)(j)=board(i,j))
  // The abstract DTI is implicitly inherited from Partial.

  /** Initialise from a file. */
  def init(fname:String) = {
    val lines = scala.io.Source.fromFile(fname).getLines()
    var i = 0 // row number
    for(line <- lines){
      // var j = 0
      // while(j < 9){
      for(j <- columns){
	val c = line.charAt(j)
	if(c.isDigit) contents(i)(j) = c.asDigit
	else if(c == '.') contents(i)(j) = 0 
        else{ println("Illegal character in file: "+c); sys.exit() }
        // j += 1
      }
      i += 1
    }
  }

  /** Print the partial solution. */
  def printPartial = {
    for(i <- rows) println(contents(i).mkString)
    println()
  }

  /** Is the partial solution complete? */
  def complete : Boolean = {
    var result = true; var i = 0
    // Inv: result is true if all entries so far have been filled
    while(result && i < 9){
      var j = 0
      while(j < 9 && contents(i)(j) != 0) j += 1
      result = (j == 9); i += 1
    }
    result
  }

  /** Copy contents into p.contents. */
  protected def copyContents(p: BasicPartial) = {
    for(i1 <- rows; j1 <- columns) p.contents(i1)(j1) = contents(i1)(j1)
  }
}

// -------------------------------------------------------

/** A simple implementation of partial solutions. */
class SimplePartial extends BasicPartial{
  /** Find a blank position; precondition: complete returns false. */
  def nextPos: Coord = {
    for(i <- rows; j <- columns) if(contents(i)(j) == 0) return (i,j);
    throw new RuntimeException("No blank position")
  }

  /** Find a blank position, and the number of options for playing there. */
  def nextPosWithCost: (Coord, Int) = ???

  /** Can we play value d in position (i,j); precondition: (i,j) is blank. */
  def canPlay(i: Int, j: Int, d: Int) : Boolean = {
    for((i1, j1) <- neighbours(i)(j)) if(contents(i1)(j1) == d) return false
    true
  }

  /** Create a new partial solution, extending this one by playing d in
    * position (i,j). */
  def play(i: Int, j: Int, d: Int) : Partial = {
    val p = new SimplePartial; copyContents(p); p.contents(i)(j) = d; p
  }

  /** Extend this partial solution by playing d in position (i,j). */
  def playHere(i: Int, j: Int, d: Int) = ???
}

