package thinkingAboutPrograms.heaps

class HeapSort(heap: Array[Int]){
  /** The size of the array. */
  private val N = heap.length

  /** The parent of the entry in position i of heap. */
  private def parent(i: Int) = (i-1)/2

  /** The left child of the entry in position i of heap. */
  private def lChild(i: Int) = 2*i+1

  /** The right child of the entry in position i of heap. */
  private def rChild(i: Int) = 2*i+2

  /** Swap entries in positions i and j. */
  private def swap(i: Int, j: Int) = {
    val t = heap(i); heap(i) = heap(j); heap(j) = t 
  }

  /** Control variable indicating how much of heap satisfies the heap
    * property. */
  private var n = 1

  /* heap[0..n) represents a max heap.  Define 
   * heapProperty(i) = heap(i) <= heap(parent(i)). */

  /** Re-establish the heap property by moving heap(n) up.  
    * Pre: n > 0 && heap[0..n) is a heap.  
    * Post: heap[0..heap+1) is a heap. */
  private def siftUp() = {
    var i = n; var p = parent(i)
    // Invariant: p = parent(i) and heap[0..n+1) is a heap except possibly
    // at positions p and i: 
    // for all j <= n, if j != i then heapProperty(j).
    while(i > 0 && heap(i) > heap(p)){
      swap(i, p)  // heapProperty(i) now holds, but heapProperty(p) might not
      i = p; p = parent(i)
    }
  }

  /** Permute £heap£ to establish the heap property. */ 
  private def makeHeap() = {
    // Invariant: £heap[0..n)£ is a heap.
    while(n < N){ siftUp(); n += 1 }
  }   

  /** Re-establish the heap property by moving heap(0) down. 
    * Pre: heap[0..n) is a heap except at 0 and its children: 
    * for all j in [3..n), heapProperty(j). */
  private def siftDown() = {
    var i = 0; var done = false
    // Invariant: heap[0..n) is a heap except at i and its children:
    // for all j in [1..n), j != lChild(i), rChild(i) => heapProperty(j).
    // done is true if heapProperty holds of both children (if existent).
    while(!done && lChild(i) < n){
      val l = lChild(i); val r = rChild(i)
      // Child with minimum cost
      val maxChild = if(r == n || heap(l) >= heap(r)) l else r
      if(heap(maxChild) > heap(i)){
	swap(i, maxChild) // swap i and maxChild
	// heap property now holds between i and its children, but might not
	// hold between minChild and its children.	
	i = maxChild
      }
      else done = true
    }
  }

  /** Sort heap using heap sort. */
  def apply() = {
    makeHeap() // Establish the heap.

    // Repeatedly swap next largest element into correct position, and 
    // re-heapify.
    // Invariant: heap[0..n) is a max heap; heap[0..n) <=
    // heap[n..N); heap[n..N) is sorted.
    while(n > 1){
      n -= 1; swap(0,n)
      // heap[0..n) <= heap[n..N); heap[n..N) is sorted;
      // heap[0..n) is a heap except possibly at 0 and its children.
      siftDown()
    }
  }
}
