package thinkingAboutPrograms.interfaces

trait Map[K, V]{
  /** Add the association key -> value to the mapping. */
  def update(key: K, value: V): Unit

  /** Get the value associated with key.
    * Pre: the key is in the mapping. */
  def apply(key: K): V

  /** Optionally get the value associated with key.
    * @return Some(x) if key is associated with x, or None if key is not in 
    * the mapping. */
  def get(key: K): Option[V]

  /** Does the mapping contain an association for key? */
  def contains(key: K): Boolean

  /** Remove key from the mapping. */
  def remove(key: K): Boolean
}
