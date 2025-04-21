package thinkingAboutPrograms.map

import thinkingAboutPrograms.interfaces.Map

/** A map from K to D, represented  using a linked list with a dummy header. */
class LinkedListHeaderMap[K,V] extends Map[K, V]{
  /** Nodes from which the list is constructed. */
  private class Node(val key: K, var value: V, var next: Node)

  /** The dummy header. */
  private var list = new Node(null.asInstanceOf[K], null.asInstanceOf[V], null)
  /* This represents the mapping composed of (n.key -> n.value) maplets, for n a
   * node reached by following 1 or more next references from list. */

  /** Find the Node before the one that contains key, or the last node in the
    * list if no such Node exists. */
  private def find(key: K) : Node = {
    var n = list
    while(n.next != null && n.next.key != key) n = n.next
    n
  }

  /** Add the maplet key -> value to the mapping. */
  def update(key: K, value: V) = {
    val n = find(key)
    if(n.next == null) list.next = new Node(key, value, list.next)
    else n.next.value = value
  }

  /** Return the value stored against key.
    * Pre: key is in the mapping. */
  def apply(key: K): V = {
    val n = find(key); require(n.next != null); n.next.value
  }

  /** Optionally get the value associated with key.
    * @return Some(x) if key is associated with x, or None if key is not in 
    * the mapping. */
  def get(key: K): Option[V] = {
    val n = find(key)
    if(n.next == null) None else Some(n.next.value)
  }

  /** Is name in the book? */
  def contains(key: K): Boolean = find(key).next != null

  /** Delete the value stored for key (if it exists); return true if the
    * key existed in the map. */
  def remove(key: K) : Boolean = {
    val n = find(key);
    if(n.next != null){ n.next = n.next.next; true }
    else false
  }
}
