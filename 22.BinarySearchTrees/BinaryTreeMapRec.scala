package thinkingAboutPrograms.binaryTrees

import scala.math.Ordered

/** A map from K to V, implemented using a binary tree, with operations 
  * implemented recursively. */
class BinaryTreeMapRec[K, V](implicit ord: K => Ordered[K])
extends thinkingAboutPrograms.interfaces.Map[K,V]{
  /** The type of nodes from which trees are constructed. */
  private class Node(
    var key: K, var value: V, var left: Node, var right: Node)

  /** The root of the tree. */
  private var root : Node = null
  /* This represents the mapping that maps n.key to n.value, for each node n
   * reachable from root by following left and right references.  Datatype
   * invariant: the tree is ordered by key fields. */

  /** Find the value stored for a particular key.   */
  def get(key: K) : Option[V] = get(key, root)

  /** Find the value stored for a particular key, within t. */
  private def get(key: K, t: Node) : Option[V] =
    if(t == null) None
    else if(key == t.key) Some(t.value)
    else if(key < t.key) get(key, t.left)
    else get(key, t.right)    

  /** Get the value associated with key.
    * Pre: key is in the domain of the mapping. */
  def apply(key: K): V = apply(key, root)

  /** Get the value associated with key in t.
    * Pre: key is in the tree rooted at t. */
  private def apply(key: K, t: Node): V = {
    assert(t != null)
    if(key == t.key) t.value
    else if(key < t.key) apply(key, t.left)
    else apply(key, t.right) 
  }

  /** Does the mapping contain key? */
  def contains(key: K): Boolean = contains(key, root)

  /** Does the tree rooted at t contain key? */
  private def contains(key: K, t: Node): Boolean = 
    t != null && 
    (t.key == key || key < t.key && contains(key, t.left) || 
     key > t.key && contains(key, t.right))

  /** A helper function to create a node with null left and right fields. */
  private def mkNode(key: K, value: V) = new Node(key, value, null, null)

  /** Add the association key -> value to the mapping. */
  def update(key: K, value: V) : Unit = root = updateRec(key, value, root)

  /** Add the association key -> value within t, returning the new tree.
    * This version updates existing nodes. */
  private def updateRec(key: K, value: V, t: Node) : Node = 
    if(t == null) mkNode(key, value)
    else if(key == t.key){ t.value = value; t }
    else if(key < t.key){ t.left = updateRec(key, value, t.left); t }
    else{ t.right = updateRec(key, value, t.right); t }

  /** Delete key from the tree.  */
  def remove(key: K) : Boolean = {
    val (n, b) = remove(key, root); root = n; b
  }

  /** Delete key from t, returning the resulting tree. */
  private def remove(key: K, t: Node) : (Node, Boolean) =
    if(t == null) (null, false)
    else if(key < t.key){
      val (l, b) = remove(key, t.left); t.left = l; (t, b)
    }
    else if(key > t.key){
      val (r, b) = remove(key, t.right); t.right = r; (t, b)
    }
    // delete the contents of this node
    else (removeRootRec(t), true)

  /** Delete the root node from t, returning a tree with the remaining keys. */
  private def removeRootRec(t: Node): Node =
    if(t.left == null) t.right
    else if(t.right == null) t.left
    else{ 
      val(w, v, newR) = removeMin(t.right)
      t.key = w; t.value = v; t.right = newR
      t
    }

  /** Delete the minimum node of t, returning the key and value from that
    * node, and the resulting tree.  Precondition: t != null. */
  private def removeMin(t:Node) : (K, V, Node) =
    if(t.left == null) (t.key, t.value, t.right)
    else{
      val (w, v, newL) = removeMin(t.left)
      t.left = newL; (w, v, t)
    }

  /** Print the contents of the tree. */
  def print = printTree(root)

  /** Print the contents of tree t. */
  private def printTree(t: Node) : Unit = 
    if(t != null){ 
      printTree(t.left); println(t.key.toString+" -> "+t.value)
      printTree(t.right)
    }
}
