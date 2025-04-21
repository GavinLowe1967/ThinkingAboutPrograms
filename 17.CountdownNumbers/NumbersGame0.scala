package thinkingAboutPrograms.countdown

import scala.util.Random

object NumbersGame0{
  // Factory methods. 
  private def IntExpression(n: Int) = new IntExpression(n)
  private def AddExpression(e1: Expression, e2: Expression) = 
    new AddExpression(e1, e2)
  private def SubExpression(e1: Expression, e2: Expression) = 
    new SubExpression(e1, e2)
  private def MultExpression(e1: Expression, e2: Expression) = 
    new MultExpression(e1, e2)
  private def DivExpression(e1: Expression, e2: Expression) = 
    new DivExpression(e1, e2)

  /** Split nums into two lists in all possible ways.
    * Pre: nums.nonEmpty. */
  private def allSplits(nums: List[Int]): List[(List[Int], List[Int])] = {
    require(nums.nonEmpty)
    if(nums.length == 1) 
      List((List(nums.head), List()), (List(nums.head), List()))
    else{
      val x = nums.head; val splits0 = allSplits(nums.tail)
      (for((xs, ys) <- splits0) yield (x::xs, ys))++
      (for((xs, ys) <- splits0) yield (xs, x::ys))
    }
  }

  private type ExpList = List[Expression]

  /** Create all expressions using all the values in nums. */ 
  private def allExpressions(nums: List[Int]): ExpList = {
    require(nums.nonEmpty)
    if(nums.length == 1) List(IntExpression(nums.head)) 
    else
      for((xs, ys) <- allSplits(nums); if xs.nonEmpty && ys.nonEmpty;
	  es <- allCombinations(xs,ys))
      yield es
  }  

  /** Create all binary expressions where the first sub-expression uses
    * variables ns1, and the second sub-expression uses variables ns2. */
  private def allCombinations(ns1: List[Int], ns2: List[Int]): ExpList = {
    val es1 = allExpressions(ns1); val es2 = allExpressions(ns2)
    for(e1 <- es1; e2 <- es2; es <- allBinaries(e1, e2)) yield es
  }

  /** Create all binary expressions combining e1 and e2. */
  private def allBinaries(e1: Expression, e2: Expression): ExpList = {
    val n1 = e1.value; val n2 = e2.value
    List( 
      AddExpression(e1,e2), MultExpression(e1,e2) // ,
      // AddExpression(e2,e1), MultExpression(e2,e1) 
    )++
    (if(n1 > n2) List(SubExpression(e1,e2))
     else if(n1 == n2) List(SubExpression(e1,e2), SubExpression(e2,e1))
     else List(SubExpression(e2,e1)))++
    (if(n2 != 0 && n1%n2 == 0) List(DivExpression(e1,e2)) else List())++
    (if(n1 != 0 && n2%n1 == 0) List(DivExpression(e2,e1)) else List())
  }
    
  /** Pick random numbers. */
  private def pickNumbers: (List[Int], Int) = {
    val numLarge = Random.nextInt(5) // number of large numbers
    val nums = 
      (for(i <- 0 until 6-numLarge) yield Random.nextInt(10)+1)++
      (for(i <- 0 until numLarge) yield (Random.nextInt(4)+1)*25)
    (nums.toList.sorted, Random.nextInt(1000)+1)
  }

  /** All subsequences of nums. */
  private def subseqs(nums: List[Int]): List[List[Int]] =
    if(nums.isEmpty) List(List())
    else{
      val xss = subseqs(nums.tail); val x = nums.head
      xss ++ xss.map(x::_) 
    }

  /** Solve a Countdown numbers round, to try to make target from nums. */
  private def solve(nums: List[Int], target: Int): Expression = {
    val subs1 = subseqs(nums)
    val subs2 = subs1.filter(_.nonEmpty) // .distinct
    val allExprs = subs2.flatMap(allExpressions)
    // println(allExprs.size); println(allExprs.mkString("\n"))
    allExprs.minBy{ e => Math.abs(e.value-target) }
  }
  
  def main(args: Array[String]) = {
    val (nums, target) =
      if(args.isEmpty) pickNumbers
      else (args.init.map(_.toInt).toList.sorted, args.last.toInt)
    println(nums, target)
    val e = solve(nums, target)
    println(e.toString+" = "+e.value)
  }
}
