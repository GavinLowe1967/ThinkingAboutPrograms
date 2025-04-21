package thinkingAboutPrograms.graphSearch

import scala.collection.mutable.HashSet

import thinkingAboutPrograms.hashMap.HashMap
import thinkingAboutPrograms.interfaces.PriorityQueue
import thinkingAboutPrograms.heaps.HeapPriorityQueue

/** Initial implementation of Dijkstra's Algorithm, using Ints as nodes. */
// class Dijkstra0(g: Graph[Int], size: Int){
  
//   private val Infinity = Double.MaxValue

//   def apply(): Option[List[Int]] = {
//     // The minimum distance found to each node so far
//     val dist = Array.fill(size)(Infinity); dist(g.init) = 0
//     // Predecessor in the shortest path found
//     val pred = Array.fill(size)(-1)
//     // Open nodes
//     var open = new HashSet[Int]; open.add(g.init) 
//     var done = false

//     while(!done && open.nonEmpty){
//       val n = open.minBy(n1 => dist(n1))
//       if(n == g.target) done = true
//       else{
// 	open.remove(n); val dn = dist(n)
// 	for((v,l) <- g.succs(n)){
// 	  val altDist = dn + l // distance to v via n
// 	  if(altDist < dist(v)){
// 	    if(dist(v) == Infinity) open.add(v)
// 	    dist(v) = altDist; pred(v) = n
// 	  }
// 	}
//       }
//     }
    
//     if(done){
//       // Recreate path
//       println(dist(g.target))
//       var n = g.target; var path = List[Int](n)
//       while(n != g.init){ n = pred(n); path ::= n }
//       Some(path)
//     }
//     else None
//   }
// }

// =======================================================

/** Implementation using generic graphs but no priority queue. */
class Dijkstra1[N](g: Graph[N]){

  def apply(): Option[List[N]] = {
    // The minimum distance found to each node so far
    val dist = new HashMap[N,Double]; dist.update(g.init, 0)
    // Predecessor in the shortest path found
    val pred = new HashMap[N,N]
    // Open nodes
    var open = new HashSet[N]; open.add(g.init)
    var done = false

    while(!done && !open.isEmpty){
      val n = open.minBy(n1 => dist(n1))
      if(n == g.target) done = true
      else{
	open.remove(n); val dn = dist(n)
	for((v,l) <- g.succs(n)){
	  val altDist = dn + l // distance to v via n
	  val isNew = !dist.contains(v) // is this the first time we've seen v?
	  if(isNew || altDist < dist(v)){
	    if(isNew) open.add(v) // else open.requeue(v, altDist)
	    dist.update(v, altDist); pred.update(v, n) 
	  }
	}
      }
    }
    
    if(done){
      println(dist(g.target))
      // Recreate path
      var n = g.target; var path = List[N](n)
      while(n != g.init){ n = pred(n); path ::= n }
      Some(path)
    }
    else None
  }
}


// =======================================================

/** Implementation using a priority queue. */
class Dijkstra[N](g: Graph[N], verbose: Boolean = false)
    extends GraphSearcher[N]{

  def apply(): Option[List[N]] = {
    // The minimum distance found to each node so far
    val dist = new HashMap[N,Double]; dist.update(g.init, 0.0)
    // Predecessor in the shortest path found
    val pred = new HashMap[N,N]
    // Open nodes
    var open: PriorityQueue[N] = new HeapPriorityQueue[N](); open.add(g.init, 0)
    var done = false
    var count = 0

    while(!done && !open.isEmpty){
      count += 1
      val n = open.get()
      if(n == g.target) done = true
      else{
        val dn = dist(n)
	for((v,l) <- g.succs(n)){
	  val altDist = dn + l // distance to v via n
	  val isNew = !dist.contains(v) // is this the first time we've seen v?
	  if(isNew || altDist < dist(v)){
	    if(isNew) open.add(v, altDist) else open.requeue(v, altDist)
	    dist.update(v, altDist); pred.update(v, n) 
	  }
	}
      }
    }

    if(verbose) println(s"count = ${count}")
    if(done){
      // println(dist(g.target))
      // Recreate path
      var n = g.target; var path = List[N](n)
      while(n != g.init){ n = pred(n); path ::= n }
      Some(path)
    }
    else None
  }
}
