package thinkingAboutPrograms.binaryTrees

import scala.math.Ordered

/** A map from K to V, implemented using a binary tree. */
class BinaryTreeMap[K, V](implicit ord: K => Ordered[K])
extends thinkingAboutPrograms.interfaces.Map[K,V]{
  /** The type of nodes from which trees are constructed. */
  private class Node(
    var key: K, var value: V, var left: Node, var right: Node)

  /** The root of the tree. */
  private var root : Node = null
  /* This represents the mapping that maps n.key to n.value, for each node n
   * reachable from root by following left and right references.  Datatype
   * invariant: the tree is ordered by key fields. */

  /** Find the value stored for a particular key. */
  def get(key: K) : Option[V] = {
    var t = root
    // Invariant: if key is within the main tree, then it is within the tree
    // rooted at t
    while(t != null && t.key != key)
      if(key < t.key) t = t.left else t = t.right
    if(t == null) None else Some(t.value)
  }

  /** Get the value associated with key.
    * Pre: key is in the domain of the mapping. */
  def apply(key: K): V = {
    var t = root
    // Guard below throws null pointer exception if precondition does not
    // hold.
    while(t.key != key){ 
      if(key < t.key) t = t.left else t = t.right
    }
    t.value
  }

  /** Does the mapping contain key? */
  def contains(key: K): Boolean = {
    var t = root
    while(t != null && t.key != key){
      if(key < t.key) t = t.left else t = t.right
    }
    t != null
  }

  /** A helper function to create a node with null left and right fields. */
  private def mkNode(key: K, value: V) = new Node(key, value, null, null)

  /** Add the association key -> value to the mapping. */
  def update(key: K, value: V) = 
    if(root == null) root = mkNode(key, value)
    else{
      var t = root
      while(key < t.key && t.left != null || key > t.key && t.right != null)
	if(key < t.key) t = t.left else t = t.right
      if(t.key == key) t.value = value
      // In the cases below, t.left, resp. t.right, is null
      else if(key < t.key){
        assert(t.left == null); t.left = mkNode(key, value)
      }
      else{ assert(t.right == null); t.right = mkNode(key, value) }
    }

  /** Find the depth of the tree rooted at t. */
  def depth(t: Node): Int =
    if(t == null) 0 else depth(t.left) max depth(t.right)

  /** Delete key from the tree. */
  def remove(key: K): Boolean =
    if(root == null) false
    else if(root.key == key){ root = removeRoot(root); true }
    else{
      var t = root
      while(key < t.key && t.left != null && t.left.key != key ||
            key > t.key && t.right != null && t.right.key != key)
	if(key < t.key) t = t.left else t = t.right
      if(key < t.key){
        if(t.left != null){
          assert(t.left.key == key); t.left = removeRoot(t.left); true
        }
        else false
      }
      else{
        assert(key > t.key)
        if(t.right != null){
          assert(t.right.key == key); t.right = removeRoot(t.right); true
        }
        else false
      }
    }

  /** Delete the root node from t, returning a tree with the remaining keys. */
  private def removeRoot(t: Node): Node =
    if(t.left == null) t.right
    else if(t.right == null) t.left
    else if(t.right.left == null){ t.right.left = t.left; t.right }
    else{
      // Find minimum node in right subtree
      var t1 = t.right
      while(t1.left.left != null) t1 = t1.left
      val t2 = t1.left // t2 is the min node in t.right
      t1.left = t2.right
      t.key = t2.key; t.value = t2.value
      t
    }  
	
  /** Print the contents of the tree, using a stack. */
  def print = {
    var t = root
    val stack = new scala.collection.mutable.Stack[Node]
    // Invariant: We still need to print t; and for each tree t1 in the stack,
    // we still need to print the data in the top node, and the data in the
    // nodes of the right subtree (in the order of the stack).
    while(t != null || !stack.isEmpty){
      if(t != null){ stack.push(t); t = t.left }
      else{
	val t1 = stack.pop(); println(t1.key.toString+" -> "+t1.value)
        t = t1.right
      }
    }
  }      
}
