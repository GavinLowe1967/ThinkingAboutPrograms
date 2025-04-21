package thinkingAboutPrograms.interfaces

/** A stack storing data of type A
  * £$\State s : \Seq A$£
  * £$\Init s = [\,]$£ */
trait Stack[A]{
  /** Is the stack empty?
    * £$\Post s = s_0 \land \Returns s = [\,]$£ */
  def isEmpty: Boolean

  /** Push x onto the stack.
    * £$\Post s = [x] \append s_0$£ */
  def push(x: A): Unit

  /** Pop a value off the stack and return it.
    * £$\Pre s \ne []$£
    * £$\Post \Returns x \ST s_0 = [x] \append s$£ */
  def pop(): A 
}
