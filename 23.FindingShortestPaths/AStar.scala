package thinkingAboutPrograms.graphSearch

import thinkingAboutPrograms.hashMap.HashMap
import thinkingAboutPrograms.interfaces.PriorityQueue
import thinkingAboutPrograms.heaps.HeapPriorityQueue

/** Implementation of the AStar algorithm.
  * @param g the graph to search.
  * @param h the heuristic function. */
class AStar[N](g: Graph[N], h: N => Double)
    extends GraphSearcher[N]{

  def apply(): Option[List[N]] = {
    // The minimum distance found to each node so far
    val dist = new HashMap[N,Double]; dist.update(g.init, 0)
    // Predecessor in the shortest path found
    val pred = new HashMap[N,N]
    // Open nodes, associating each node n with cost dist(n)+h(n)
    var open: PriorityQueue[N] = new HeapPriorityQueue[N]()
    open.add(g.init, h(g.init))
    var done = false

    while(!done && !open.isEmpty){
      val n = open.get()
      if(n == g.target) done = true
      else{
        val dn = dist(n)
	for((v,l) <- g.succs(n)){
	  val altDist = dn + l // distance to v via n
	  val isNew = !dist.contains(v) // is this the first time we've seen v?
	  if(isNew || altDist < dist(v)){
            val f = altDist+h(v)
	    if(isNew) open.add(v, f) else open.requeue(v, f)
	    dist.update(v, altDist); pred.update(v, n) 
	  }
	}
      }
    }
    
    if(done){      // Recreate path
      var n = g.target; var path = List[N](n)
      while(n != g.init){ n = pred(n); path ::= n }
      Some(path)
    }
    else None
  }

}
