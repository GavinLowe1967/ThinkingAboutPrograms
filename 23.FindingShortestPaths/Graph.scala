package thinkingAboutPrograms.graphSearch

/** A weighted graph with nodes of type N. */
trait Graph[N]{
  /** The initial node. */
  val init: N

  /** The successors of n, with their distances. */
  def succs(n: N): List[(N, Double)]
  
  /** The target node. */
  val target: N
}

// =======================================================

/** A graph based on a size-by-size grid. */
class Grid(val length: Int) extends Graph[(Int,Int)]{
  type Coord = (Int,Int)

  /** The initial node. */
  val init: Coord = (0,0)

  /** The successors of n, with their distances. */
  def succs(c: Coord): List[(Coord,Double)] = {
    val (i,j) = c
    List((i+1,j), (i-1,j), (i,j+1), (i,j-1)).
      filter{ case (i1,j1) => 0 <= i1 && i1 < length && 0 <= j1 && j1 < length }.
      map(c1 => (c1,1.0)) ++
    List((i+1,j+1), (i-1,j+1), (i+1,j-1), (i-1,j-1)).
      filter{ case (i1,j1) => 0 <= i1 && i1 < length && 0 <= j1 && j1 < length }.
      map(c1 => (c1,1.5))
  }
  
  /** The target node. */
  val target: Coord = (length-1,length-1)
}

// =======================================================

/** A graph based on a size-by-size grid with Int-valued nodes. */
class IntGrid(val length: Int) extends Graph[Int]{
  type Coord = (Int,Int)

  private def toPair(n: Int) = (n/length, n%length)

  private def fromPair(c: Coord) = length*c._1 + c._2

  /** The initial node. */
  val init = fromPair((0,0))

  /** The successors of n, with their distances. */
  def succs(n: Int): List[(Int,Double)] = {
    val (i,j) = toPair(n)
    List((i+1,j), (i-1,j), (i,j+1), (i,j-1)).
      filter{ case (i1,j1) => 0 <= i1 && i1 < length && 0 <= j1 && j1 < length }.
      map(c1 => (fromPair(c1),1.0))
  }
  
  /** The target node. */
  val target = fromPair((length-1,length-1))
}


trait GraphSearcher[N]{
  def apply(): Option[List[N]]
}
