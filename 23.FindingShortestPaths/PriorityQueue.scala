package thinkingAboutPrograms.interfaces

/** A priority queue holding data of type N. */
trait PriorityQueue[N]{
  /** Is the queue empty? */
  def isEmpty: Boolean

  /** Add x to the heap with cost c. */
  def add(x: N, c: Double): Unit

  /** Get and remove a minimum cost element of the heap. */
  def get(): N

  /** Requeue value x with cost cost, no greater than the previous cost. */
  def requeue(x: N, cost: Double): Unit
}


