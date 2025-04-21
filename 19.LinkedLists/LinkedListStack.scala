package thinkingAboutPrograms.stack

import thinkingAboutPrograms.interfaces.Stack


// =======================================================

/** A stack holding data of type A, implemented using a linked list. */
class LinkedListStack[A] extends Stack[A]{
  /** Nodes used to implement the linked list. */
  private class Node(val datum: A, var next: Node)

  /** The top of the stack. */
  private var top: Node = null

  /* This represents <n.datum | n <- L(top)>. */

  /** Push x onto the stack. */
  def push(x: A) = top = new Node(x, top)

  /** Pop a value from the stack. */
  def pop(): A = {
    require(top != null); val result = top.datum; top = top.next; result
  }

  /** Is this empty? */
  def isEmpty: Boolean = top == null
}

// =============================================================================


import scala.util.Random
object StackTest{
  val iters = 1000 // ops per test

  val reps = 100000

  def doTest = {
    val llst = new LinkedListStack[Int]
    val st = new scala.collection.mutable.Stack[Int]
    for(i <- 0 until iters) Random.nextInt(3) match{
      case 0 => val x = Random.nextInt(100); llst.push(x); st.push(x)
      case 1 => 
        if(!st.isEmpty) assert(llst.pop() == st.pop())
        else assert(llst.isEmpty)
      case 2 => assert(llst.isEmpty == st.isEmpty)
    }
  }

  def main(args: Array[String]) = {
    for(r <- 0 until reps){
      doTest; if(r%1000 == 0) print(".")
    }
    println()
  }
}
