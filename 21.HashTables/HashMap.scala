package thinkingAboutPrograms.hashMap

/** A hash table using chaining, representing a mapping. */
class HashMap[K, V] extends thinkingAboutPrograms.interfaces.Map[K,V] {
  private var buckets = 4 // # buckets in the hash table; a power of 2
  private var theSize = 0 // # keys stored
  private val MaxLoadFactor = 0.75 // max load factor allowed
  private var threshold = (buckets*MaxLoadFactor).toInt // max # elements before
						   // resizing needed

  /** The type of nodes in linked lists. */
  private class Node(val key: K, var value: V, val hash: Int, var next: Node)

  /** Array holding the buckets. */
  private var table = new Array[Node](buckets) 

  /** Mask for reducing value mod buckets; always equals buckets-1. */
  private var mask = buckets-1 

  /** Get the index of the bucket in which to store a key with hash code h. */
  def index(h: Int) = h & mask

  /** Find node containing key and hash in linked list starting at head, or
    * return null if key and hash do not appear. */
  private def find(key: K, hash: Int, head: Node): Node = {
    var n = head
    while(n != null && (n.hash != hash || n.key != key)) n = n.next
    n
  }

  /** Add the association key -> value to the table. */
  def update(key: K, value: V) = {
    if(theSize >= threshold) resize()
    val hash = key.hashCode; val i = index(hash)
    val n = find(key, hash, table(i))
    if(n != null){ assert(n.key == key); n.value = value }
    else{ table(i) = new Node(key, value, hash, table(i)); theSize += 1 }
  }

  /** Find the value stored for key. */
  def get(key: K): Option[V] = {
    val hash = key.hashCode; val n = find(key, hash, table(index(hash)))
    if(n != null){ assert(n.key == key); Some(n.value) } else None
  }

  /** Does the mapping contain key? */
  def contains(key: K): Boolean = {
    val hash = key.hashCode; find(key, hash, table(index(hash))) != null
  }

  /** Find the value stored for key.  Pre: the key is in the mapping. */
  def apply(key: K): V = {
    val hash = key.hashCode; val n = find(key, hash, table(index(hash)))
    assert(n != null && n.key == key); n.value
  }

  /** Delete the value associated with key. */
  def remove(key: K): Boolean = {
    val hash = key.hashCode; val i = index(hash); var n = table(i)
    if(n == null) false
    else if(n.hash == hash && n.key == key){
      table(i) = n.next; theSize -= 1; true
    }
    else{
      var p = n; n = p.next // Inv: n = p.next
      while(n != null && (n.hash != hash || n.key != key)){
        p = n; n = n.next
      }
      if(n != null){ p.next = n.next; theSize -= 1; true }
      else false
    }
  }

  /** Delete the value associated with key. */
  // def delete1(key: K): Option[V] = {
  //   val hash = key.hashCode; val i = index(hash); var n = table(i)
  //   if(n == null) None
  //   else if(n.hash == hash && n.key == key){
  //     table(i) = n.next; theSize -= 1; Some(n.value)
  //   }
  //   else{
  //     var p = n; n = p.next // Inv: n = p.next
  //     while(n != null && (n.hash != hash || n.key != key)){
  //       p = n; n = n.next
  //     }
  //     if(n != null){ p.next = n.next; theSize -= 1; Some(n.value) }
  //     else None
  //   }
  // }

  // return the size
  def size = theSize

  // print the hash table
  def print = {
    for(i <- 0 until buckets){
      var n = table(i)
      while(n != null){ println(n.key.toString+" -> "+n.value); n = n.next }
    }
  }

  /** resize the hash table, by doubling its size. */
  private def resize(): Unit = { 
    // println("Resizing "+buckets)
    val oldBuckets = buckets; buckets = 2*buckets
    threshold = (buckets*MaxLoadFactor).toInt; mask = buckets-1
    val oldTable = table; table = new Array[Node](buckets)
    // copy entries
    for(i <- 0 until oldBuckets){
      var n = oldTable(i) // iterate through this list, copying entries
      while(n != null){
	val j = index(n.hash); val next = n.next
        n.next = table(j); table(j) = n; n = next
      }
    }
  }
}
