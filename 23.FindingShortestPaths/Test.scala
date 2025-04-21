package thinkingAboutPrograms.graphSearch

object Test{
  def main(args: Array[String]) = {
    var aStar = false; var size = 100
    var i = 0
    while(i < args.length) args(i) match{
      case "--AStar" => aStar = true; i += 1
      case "--size" => size = args(i+1).toInt; i += 2
    }

    val g = new Grid(size)
    // Heuristic function for AStar
    def h(c: g.Coord): Double = {
      val (x,y) = c; val (tx,ty) = g.target; val dx = tx-x; val dy = ty-y
      // Math.abs(dx)+Math.abs(dy)
      Math.sqrt(dx*dx+dy*dy)
    }
    val searcher = if(!aStar) new Dijkstra(g) else new AStar(g, h)
    searcher() match{
      case Some(path) => println(path)
      case None => println("No path found")
    }
  }
}
