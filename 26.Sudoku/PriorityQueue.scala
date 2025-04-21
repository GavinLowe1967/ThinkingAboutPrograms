package thinkingAboutPrograms.sudoku

import Partial.Coord


object PriorityQueue{
  type Heap = Array[(Coord,Int)]

  /** A map representing where each coordinate is stored, as a flattened array
    * with 81 entries.  The entry for coordinate (i,j) is in location 9*i+j.
    * Negative entries represent that the coordinate is not stored. */
  type LocMap = Array[Int]
}

// ==================================================================

import PriorityQueue._

/** A priority queue using a heap.  This combines the common code between
  * PriorityQueue and TacticPriorityQueue. */
trait PriorityQueue{
  /* Array holding the elements of the queue, together with their costs. */ 
  protected val heap: Heap

  /* The number of elements in the queue. */
  protected var length: Int

  /** The current size of the heap array. */
  protected var size = heap.length 

  /* heap[0..length) represents a min heap.  Define 
   * heapProperty(i) = cost(i) >= cost(parent(i))
   * Invariant: For each i in [1..length), heapProperty(i). */

  require(length <= size)

  /** The value in heap(i). */
  protected def valueAt(i: Int) = heap(i)._1

  /** The cost associated with the value in heap(i). */
  protected def cost(i: Int) = heap(i)._2

  /** The parent of the entry in position i of heap. */
  protected def parent(i: Int) = (i-1)/2

  /** The left child of the entry in position i of heap. */
  protected def lChild(i: Int) = 2*i+1

  /** The right child of the entry in position i of heap. */
  protected def rChild(i: Int) = 2*i+2

  /* Each implementing class should provide a "location map", showing where in
   * the heap a value of type Coord is stored, together with implementations of
   * the following two operations. */

  /** Update the location map to record that x is stored at loc. */
  protected def updateLocMap(x: Coord, loc: Int): Unit

  /** Get the location in heap where x is stored. */
  protected def locMapGet(x: Coord): Int

  /** Swap entries in positions i and j. */
  protected def swap(i: Int, j: Int) = {
    val t = heap(i); heap(i) = heap(j); heap(j) = t 
    updateLocMap(valueAt(i), i); updateLocMap(valueAt(j), j)
  }

  /** Is the queue empty? */
  def isEmpty = length == 0

  /** Re-establish the heap property by moving heap(i) up.
    * Pre: the heap property holds everywhere except possibly at i0 and 
    * parent(i0): for all j in [0..length), j != i0 => heapProperty(j). */
  protected def siftUp(i0: Int) = {
    var i = i0; var p = parent(i)
    // Invariant: p = parent(i) and heap[0..length) is a heap except possibly
    // at positions p and i: for all j != i, heapProperty(j).
    while(i > 0 && cost(i) < cost(p)){
      swap(i, p)  // heapProperty(i) now holds, but heapProperty(p) might not
      i = p; p = parent(i)
    }
  }

  /** Add x to the heap with cost c. */
  def add(x: Coord, c: Int) = {
    require(locMapGet(x) < 0); require(length < size)
    heap(length) = (x,c); updateLocMap(x, length) 
    siftUp(length); length = length+1
  }

  /** Re-establish the heap property by moving heap(0) down. 
    * Pre: heap[0..length) is a heap except at 0 and its children: 
    * for all j in [3..length), heapProperty(j). */
  protected def siftDown(i0: Int = 0) = {
    var i = i0; var done = false
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

  /** Remove x from the top of the heap. */
  @inline private def removeTop(x: Coord) = { 
    assert(locMapGet(x) == 0)
    updateLocMap(x, -1); length = length-1
    // Overwrite x with last value, and re-form heap
    if(length > 0){
      heap(0) = heap(length); updateLocMap(valueAt(0), 0); siftDown()
    }
  }

  /** Get and remove a minimum cost element of the heap. */
  def get(): Coord = {
    require(length != 0); val (result,c) = heap(0); removeTop(result); result
  }

  /** Remove a minimum cost element of the heap.  Return that element and its
    * cost. */
  def getWithCost(): (Coord, Int) = {
    require(length != 0); val pair = heap(0); removeTop(pair._1); pair
  }

  /** Decrement the cost associated with x. */
  def decrementCost(x: Coord) = {
    // Find location of x
    val i = locMapGet(x); assert(0 <= i && i < heap.length, s"$x $i")
    val (y,c) = heap(i); assert(y == x && c > 0)
    // Update and rebuild the heap
    heap(i) = (x,c-1); siftUp(i)
  }

  override def clone: PriorityQueue = ???
  /* Note: this declaration seems necessary to keep the compiler happy.  Each
   * subclass must have a clone method that produces a HeapPriorityQueue; but
   * the definition here must be concrete to override the definition in
   * Object. */

  override def toString = 
    (for(i <- 0 until length) yield heap(i)).mkString("\t")
}

// =======================================================

import scala.collection.mutable.HashMap

class HashMapPriorityQueue(
  protected var length: Int, protected val heap: Heap, 
  location: HashMap[Coord, Int])
    extends PriorityQueue{

  /** Subsidiary constructor, to create an empty priority queue with capacity
    * initSize. */
  def this(initSize: Int) = this(0, new Heap(initSize), new HashMap[Coord,Int])

  /** Update locMap to record that coord is stored at loc. */
  @inline protected def updateLocMap(pair: Coord, loc: Int) = 
    location(pair) = loc

  /** Get the location in heap where pair is stored, or -1 if it isn't there. */
  @inline protected def locMapGet(pair: Coord): Int = location.get(pair) match{
    case Some(ix) => ix; case None => -1
  }

  /** Clone this. */
  override def clone = 
    new HashMapPriorityQueue(length, heap.clone, location.clone)
}
 
// ==================================================================

import PriorityQueue.LocMap

/** A priority queue holding data of type N, based on a heap, and using a
  * HeapPriorityQueue.LocMap to store locations.  This corresponds to an
  * exercise in the book.
  * @param length The number of elements in the queue.
  * @param heap Array holding the elements of the queue, together with their 
  * costs. 
  * @param locMap  Map giving the location of each value: if value x is in 
  * heap(i), then locMap(x) = i.*/
class LocMapPriorityQueue(
  protected var length: Int, protected val heap: Heap, locMap: LocMap)
    extends PriorityQueue{

  /** Subsidiary constructor, to create an empty priority queue with capacity
    * initSize. */
  def this(initSize: Int) = this(0, new Heap(initSize), Array.fill(81)(-1))

  @inline def indexFor(pair: Coord) = 9*pair._1+pair._2

  /** Update locMap to record that coord is stored at loc. */
  @inline protected def updateLocMap(pair: Coord, loc: Int) = 
    locMap(indexFor(pair)) = loc

  /** Get the location in heap where pair is stored. */
  @inline protected def locMapGet(pair: Coord): Int = locMap(indexFor(pair))

  /** Clone this. */
  override def clone = new LocMapPriorityQueue(length, heap.clone, locMap.clone)
}

