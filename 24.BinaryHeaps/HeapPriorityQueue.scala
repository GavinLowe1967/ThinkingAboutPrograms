package thinkingAboutPrograms.heaps

import scala.collection.mutable.HashMap 
import thinkingAboutPrograms.interfaces.PriorityQueue

/** A priority queue holding data of type N, based on a heap.
  * @param length The number of elements in the queue.
  * @param heap Array holding the elements of the queue, together with their 
  * costs. 
  * @param locMap  Map giving the location of each value: if value x is in 
  * heap(i), then locMap(x) = i.*/
class HeapPriorityQueue[N](
  private var length: Int, 
  private var heap: Array[(N,Double)], 
  locMap: HashMap[N, Int])
    extends PriorityQueue[N]{

  /** Subsidiary constructor, to create an empty priority queue with capacity
    * initSize. */
  def this(initSize: Int = 16) = 
    this(0, new Array[(N,Double)](initSize), new HashMap[N, Int])

  /** The current size of the heap array. */
  private var size = heap.length

  require(length <= size)

  /** The value in heap(i). */
  private def valueAt(i: Int) = heap(i)._1

  /** The cost associated with the value in heap(i). */
  private def cost(i: Int) = heap(i)._2

  /** The parent of the entry in position i of heap. */
  private def parent(i: Int) = (i-1)/2

  /** The left child of the entry in position i of heap. */
  private def lChild(i: Int) = 2*i+1

  /** The right child of the entry in position i of heap. */
  private def rChild(i: Int) = 2*i+2

  /* heap[0..length) represents a min heap.  Define 
   * heapProperty(i) = cost(i) >= cost(parent(i))
   * Invariant: For each i in [1..length), heapProperty(i). */

  /** Swap entries in positions i and j. */
  private def swap(i: Int, j: Int) = {
    val t = heap(i); heap(i) = heap(j); heap(j) = t 
    locMap.update(valueAt(i), i); locMap.update(valueAt(j), j)
  }

  /** Is the queue empty? */
  def isEmpty = length == 0

  /** Add n to the heap with cost c. */
  def add(n: N, c: Double) = {
    require (!locMap.contains(n))
    if(length == size) resize()
    heap(length) = (n,c); locMap.update(n, length)
    siftUp(length); length += 1
  }

  /** Resize heap. */
  private def resize() = { 
    val newHeap = new Array[(N,Double)](2*size)
    for(i <- 0 until size) newHeap(i) = heap(i) 
    heap = newHeap; size = 2*size
  }

  /** Re-establish the heap property by moving heap(i) up.
    * Pre: the heap property holds everywhere except possibly at i0 and 
    * parent(i0): for all j in [0..length), j != i0 => heapProperty(j). */
  private def siftUp(i0: Int) = {
    var i = i0; var p = parent(i)
    // Invariant: p = parent(i) and heap[0..length) is a heap except possibly
    // at positions p and i: for all j != i, heapProperty(j).
    while(i > 0 && cost(i) < cost(p)){
      swap(i, p)  // heapProperty(i) now holds, but heapProperty(p) might not
      i = p; p = parent(i)
    }
  }

  /** Get and remove a minimum cost element of the heap. */
  def get(): N = {
    require(length != 0)
    val (result,_) = heap(0); assert(locMap(result) == 0)
    locMap.remove(result); length -= 1
    // Overwrite result with last value, and re-form heap
    if(length > 0){
      heap(0) = heap(length); locMap.update(valueAt(0), 0); siftDown()
    }
    result
  }

  /** Re-establish the heap property by moving heap(0) down. 
    * Pre: heap[0..length) is a heap except at 0 and its children: 
    * for all j in [3..length), heapProperty(j). */
  private def siftDown() = {
    var i = 0; var done = false
    // Invariant: heap[0..length) is a heap except at i and its children:
    // for all j in [1..length), j != lChild(i), rChild(i) => heapProperty(j).
    // done is true if heapProperty holds of both children (if existent).
    while(!done && lChild(i) < length){
      val l = lChild(i); val r = rChild(i)
      // Child with minimum cost
      val minChild = if(r == length || cost(l) <= cost(r)) l else r
      if(cost(minChild) < cost(i)){
	// swap i and minChild
	swap(i, minChild) 
	// heap property now holds between i and its children, but might not
	// hold between minChild and its children.	
	i = minChild
      }
      else done = true
    }
  }

  /** Requeue value x with cost cost, no greater than the previous cost. */
  def requeue(x: N, c: Double) = {
    // Find location of x
    val i = locMap(x); val (y,c1) = heap(i); assert(y == x && c <= c1)
    // Update and rebuild the heap
    heap(i) = (x,c); siftUp(i)
  }

  /** Clone this. */
  override def clone: HeapPriorityQueue[N] = 
    new HeapPriorityQueue[N](length, heap.clone, locMap.clone)

  override def toString = 
    (for(i <- 0 until length) yield heap(i)).mkString("\t")
}

// =======================================================

/** A priority queue holding data of type N, based on a heap.
  * @param maxSize the maximum size of the queue. */
class HeapPriorityQueue0[N](maxSize: Int = 16) extends PriorityQueue[N]{ 

  /** Array holding the elements of the queue, together with their costs. */
  private var heap = new Array[(N,Double)](maxSize)

  /** The number of elements in the queue. */
  private var length = 0

  /** The cost associated with the value in heap(i). */
  private def cost(i: Int) = heap(i)._2

  /** The parent of the entry in position i of heap. */
  private def parent(i: Int) = (i-1)/2

  /** The left child of the entry in position i of heap. */
  private def lChild(i: Int) = 2*i+1

  /** The right child of the entry in position i of heap. */
  private def rChild(i: Int) = 2*i+2

  /* heap[0..length) represents a min heap.  Define 
   * heapProperty(i) = cost(i) >= cost(parent(i))
   * Invariant: For each i in [1..length), heapProperty(i). */

  /** Swap entries in positions i and j. */
  private def swap(i: Int, j: Int) = {
    val t = heap(i); heap(i) = heap(j); heap(j) = t 
  }

  /** Is the queue empty? */
  def isEmpty = length == 0

  /** Add n to the heap with cost c. */
  def add(n: N, c: Double) = {
    require(length < maxSize); heap(length) = (n,c); siftUp; length += 1; 
  }

  /** Re-establish the heap property by moving heap(i) up.
    * Pre: the heap property holds everywhere except possibly at length:
    * for all j in [0..length), j != i0 => heapProperty(j). */
  private def siftUp = {
    var i = length; var p = parent(i)
    // Invariant: p = parent(i) and heap[0..length) is a heap except possibly
    // at positions p and i: for all j != i, heapProperty(j).
    while(i > 0 && cost(i) < cost(p)){
      swap(i, p)  // heapProperty(i) now holds, but heapProperty(p) might not
      i = p; p = parent(i)
    }
  }

  /** Get and remove a minimum cost element of the heap. */
  def get(): N = {
    require(length != 0)
    val (result,_) = heap(0); length -= 1
    // Overwrite result with last value, and re-form heap
    if(length > 0){ heap(0) = heap(length); siftDown }
    result
  }

  /** Re-establish the heap property by moving heap(0) down. 
    * Pre: heap[0..length) is a heap except at 0 and its children: 
    * for all j in [3..length), heapProperty(j). */
  private def siftDown = {
    var i = 0; var done = false
    // Invariant: heap[0..length) is a heap except at i and its children:
    // for all j in [1..length), j != lChild(i), rChild(i) => heapProperty(j).
    // done is true if heapProperty holds of both children (if existent).
    while(!done && lChild(i) < length){
      val l = lChild(i); val r = rChild(i)
      // Child with minimum cost
      val minChild = if(r == length || cost(l) <= cost(r)) l else r
      if(cost(minChild) < cost(i)){
	// swap i and minChild
	swap(i, minChild) 
	// heap property now holds between i and its children, but might not
	// hold between minChild and its children.	
	i = minChild
      }
      else done = true
    }
  }

  /** Requeue value x with cost cost, no greater than the previous cost. */
  def requeue(n: N, c: Double) = ??? 
}



// =======================================================

import  thinkingAboutPrograms.util.MyTest._

object PriorityQueueTest{
  def main(args: Array[String]) = {
    val size = 10
    //val q = new HeapPriorityQueue0[Int](size)
    val q = new HeapPriorityQueue[Int](size)
    q.add(1,5)
    test("A1")(assert(q.get() == 1))
    test("A2")(assert(q.isEmpty))

    q.add(1,5); q.add(2,3); q.add(3,8); q.add(4,7); q.add(5,2); q.add(6,3)
    test("B1")(assert(q.get() == 5))
    val x = q.get(); val y = q.get()
    test("B2")(assert(x == 2 && y == 6 || x == 6 && y == 2))
    test("B3")(assert(q.get() == 1))
    test("B4")(assert(q.get() == 4))
    test("B5")(assert(q.get() == 3))
    test("B6")(assert(q.isEmpty))

    // Now include requeues
    q.add(1,5); q.add(2,3); q.add(3,8); q.add(4,7); q.add(5,1); q.add(6,3)
    q.requeue(1, 2)
    test("C1")(assert(q.get() == 5))
    test("C2")(assert(q.get() == 1))
    q.requeue(3,6)
    val x1 = q.get(); val y1 = q.get()
    test("C3")(assert(x1 == 2 && y1 == 6 || x1 == 6 && y1 == 2))
    test("C4")(assert(q.get() == 3))
    q.requeue(4,4); q.add(1,6); q.add(2,1)
    test("C5")(assert(q.get() == 2))
    test("C6")(assert(q.get() == 4))
    test("C7")(assert(q.get() == 1))
    test("C8")(assert(q.isEmpty))

    () 
  }

}
