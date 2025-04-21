package thinkingAboutPrograms.hashMap

import scala.reflect.ClassTag

/** A hash table, representing a mapping.
  * This version does not support deletion. */
class OpenHashMap0[K: ClassTag, V: ClassTag]
    extends thinkingAboutPrograms.interfaces.Map[K,V] {
  private var buckets = 4 // # slots in the hash table; a power of 2
  private var theSize = 0 // # keys stored
  private val MaxLoadFactor = 0.6 // max load factor allowed

  /** Maximum number of elements before resizing needed. */
  private var threshold = (buckets*MaxLoadFactor).toInt

  /** Array holding hashes. */
  private var hashes = new Array[Int](buckets)

  /** Array holding keys. */
  private var keys = new Array[K](buckets)

  /** Array of booleans indicating which slots are filled. */
  private var filled = new Array[Boolean](buckets)

  /** Array holding values. */
  private var values = new Array[V](buckets)

  /* This represents the keys(i) -> values(i) maplets for which filled(i) is
   * true. 
   *   { keys(i) -> values(i) | i <- [0..buckets), filled(i) }. */

  /** Mask for reducing value mod buckets; always equals buckets-1. */
  private var mask = buckets-1 

  /** Get the index of the bucket in which to store a key with hash code h. */
  def index(h: Int) = h & mask

  /* Each value k -> v will be stored as soon as possible after index(hash(k)).
   * Hence if this is stored in position i, all entries in [index(hash(k))..i)
   * (wrapping round) will be filled. */

  /** Find the location where key is stored or could be stored.  
    * @return i such that keys(i) = key, or first free space after index(h) if 
    * key does not appear (in which case !filled(i)). */
  private def find(key: K, h: Int): Int = {
    var i = index(h)
    while(filled(i) && (hashes(i) != h || keys(i) != key)) i = (i+1)&mask
    i
  }

  /** Add the association key -> value to the table. */
  def update(key: K, value: V) = {
    if(theSize >= threshold) resize()
    val h = key.hashCode; val i = find(key, h)
    if(filled(i)) assert(hashes(i) == h && keys(i) == key) 
    else{ hashes(i) = h; keys(i) = key; filled(i) = true; theSize += 1 }
    values(i) = value
  }

  /** Find the value stored for key.  Pre: the key is in the mapping. */
  def apply(key: K): V = {
    val h = key.hashCode; val i = find(key, h)
    assert(filled(i) && hashes(i) == h && keys(i) == key)
    values(i)
  }

  /** Does the mapping contain key? */
  def contains(key: K): Boolean = {
    val h = key.hashCode; val i = find(key, h); filled(i)
  }

  /** Find the value stored for key. */
  def get(key: K): Option[V] = {
    val h = key.hashCode; val i = find(key, h)
    if(!filled(i)) None
    else{ assert(hashes(i) == h && keys(i) == key); Some(values(i)) }
  }

  /** Delete the value associated with key. */
  def remove(key: K): Boolean =  ???

  /** Resize the table. */
  private def resize() = {
    require(buckets < (1 << 30), "maximum capacity reached")
    val oldBuckets = buckets; buckets = 2*buckets
    threshold = (buckets*MaxLoadFactor).toInt; mask = buckets-1
    val oldHashes = hashes; hashes = new Array[Int](buckets)
    val oldKeys = keys; keys = new Array[K](buckets)
    val oldValues = values; values = new Array[V](buckets)
    val oldFilled = filled; filled = new Array[Boolean](buckets)
    // Copy entries
    for(i <- 0 until oldBuckets; if oldFilled(i)){
      val key = oldKeys(i); val h = oldHashes(i); val j = find(key, h)
      assert(!filled(j)); hashes(j) = h; keys(j) = key
      values(j) = oldValues(i); filled(j) = oldFilled(i)
    }
  }

}
