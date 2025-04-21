package thinkingAboutPrograms.countdown

/** Representation of an expression. */
trait Expression{
  /** The value represented by this expression. */
  val value: Int

  /** Convert to String, with no outer-most parentheses. */
  override def toString: String

  /** Convert to string, with outer-most parentheses around AddExpressions and
    * SubExpressions. */
  def toString1: String

  /** Convert to string, with outer-most parentheses around all binary
    * sub-expressions. */
  def toString2: String

  def parenthesise(st: String) = "("+st+")"
} 

// -------------------------------------------------------

/** Expression representing the integer n. */
class IntExpression(n: Int) extends Expression{
  val value = n

  override def toString = n.toString

  def toString1 = toString

  def toString2 = toString
}

// -------------------------------------------------------

/** Expression representing the sum of e1 and e2. */
class AddExpression(e1: Expression, e2: Expression) extends Expression{
  val value = e1.value + e2.value

  override def toString = e1.toString+"+"+e2.toString

  def toString1 = parenthesise(toString)

  def toString2 = parenthesise(toString)
}

// -------------------------------------------------------

/** Expression representing the difference between e1 and e2. */
class SubExpression(e1: Expression, e2: Expression) extends Expression{
  require(e1.value >= e2.value)

  val value = e1.value - e2.value

  override def toString = e1.toString+"-"+e2.toString1

  def toString1 = parenthesise(toString)

  def toString2 = parenthesise(toString)
}

// -------------------------------------------------------

/** Expression representing the product of e1 and e2. */
class MultExpression(e1: Expression, e2: Expression) extends Expression{
  val value = e1.value * e2.value

  override def toString = e1.toString1+"*"+e2.toString1

  def toString1 = toString

  def toString2 = parenthesise(toString)
}

// -------------------------------------------------------

/** Expression representing the ratio of e1 and e2. */
class DivExpression(e1: Expression, e2: Expression) extends Expression{
  require(e2.value > 0 && e1.value % e2.value == 0)

  val value = e1.value / e2.value

  override def toString = e1.toString1+"/"+e2.toString2

  def toString1 = toString

  def toString2 = parenthesise(toString)
}
