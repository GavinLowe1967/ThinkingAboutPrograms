package thinkingAboutPrograms.countdown

import scala.util.Random

object NumbersGame{
  private def IntExpression(n: Int) = new IntExpression(n)
  private def AddExpression(e1: Expression, e2: Expression) = 
    new AddExpression(e1, e2)
  private def SubExpression(e1: Expression, e2: Expression) = 
    new SubExpression(e1, e2)
  private def MultExpression(e1: Expression, e2: Expression) = 
    new MultExpression(e1, e2)
  private def DivExpression(e1: Expression, e2: Expression) = 
    new DivExpression(e1, e2)

  /** Split nums into two lists in all possible ways, with nums.head in the
    * first list.
    * Pre: nums.nonEmpty. */
  private def allSplits(nums: List[Int]): List[(List[Int], List[Int])] = {
    require(nums.nonEmpty)
    if(nums.length == 1) List((List(nums.head), List()))
    else{
      val x = nums.head; val splits0 = allSplits(nums.tail)
      (for((xs, ys) <- splits0) yield (x::xs, ys))++
      (for((xs, ys) <- splits0) yield (x::ys, xs))
    }
  }

  private type ExpList = List[Expression]

  /** Create all expressions using all the values in nums. */ 
  private def allExpressions(nums: List[Int]): ExpList = {
    require(nums.nonEmpty)
    if(nums.length == 1){ val n = nums.head; List(IntExpression(n)) }
    else{
      mergeLists(
        for((xs, ys) <- allSplits(nums); if xs.nonEmpty && ys.nonEmpty) yield
          allCombinations(xs,ys)
      ) 
    }
  }  

  /** Create all binary expressions where the first sub-expression uses
    * variables ns1, and the second sub-expression uses variables ns2. */
  private def allCombinations(ns1: List[Int], ns2: List[Int]): ExpList = {
    val es1 = allExpressions(ns1); val es2 = allExpressions(ns2)
    mergeLists(
      for(e1 <- es1; e2 <- es2) yield mergeSort(allBinaries(e1, e2))
    )
  } 

  /** Create all binary expressions combining e1, with value n1, and e2, with
    * value n2. */
  private def allBinaries(e1: Expression, e2: Expression): ExpList = {
    val n1 = e1.value; val n2 = e2.value
    List( AddExpression(e1,e2), MultExpression(e1,e2) )++
    List(if(n1 >= n2) SubExpression(e1,e2) else SubExpression(e2,e1))++
    (if(n2 != 0 && n1%n2 == 0) List(DivExpression(e1,e2)) else List())++
    (if(n1 != 0 && n1 != n2 && n2%n1 == 0) List(DivExpression(e2,e1)) 
     else List())
  }
    
  /** Merge two ExpLists by their numeric values, removing duplicates.
    * Pre: the arguments are sorted in this way. 
    * This version is iterative, which could potentially avoid stack 
    * overflows from the recursive case. */
  private def merge(es1: ExpList, es2: ExpList): ExpList = {
    var result = List[Expression]()
    var xs = es1; var ys = es2
    while(xs.nonEmpty && ys.nonEmpty){
      val e1 = xs.head; val e2 = ys.head; val n1 = e1.value; val n2 = e2.value
      if(n1 == n2){ result ::= e1; xs = xs.tail; ys = ys.tail }
      else if(n1 < n2){ result ::= e1; xs = xs.tail }
      else{ result ::= e2; ys = ys.tail }
    }
    while(xs.nonEmpty){ result ::= xs.head; xs = xs.tail }
    while(ys.nonEmpty){ result ::= ys.head; ys = ys.tail }
    result.reverse
  }

  /** Merge two ExpLists by their numeric values, removing duplicates.
    * Pre: the arguments are sorted in this way.
    * This version can cause a stack overflow. */
   private def mergeRec(es1: ExpList, es2: ExpList): ExpList = {
    if(es1.isEmpty) es2
    else if(es2.isEmpty) es1
    else{
      val e1 = es1.head; val e2 = es2.head; 
      val n1 = e1.value; val n2 = e2.value
      if(n1 == n2)  // arbitrarily keep e1
        e1 :: mergeRec(es1.tail, es2.tail)
      else if(n1 < n2) e1 :: mergeRec(es1.tail, es2)
      else e2 :: mergeRec(es1, es2.tail)
    }
  }

  /** Sort es, removing duplicates. */
  private def mergeSort(es: ExpList): ExpList =
    if(es.length <= 1) es
    else{
      val k = es.length/2; val es1 = es.take(k); val es2 = es.drop(k)
      merge(mergeSort(es1), mergeSort(es2))
    }

  /** Merge a list of sorted ExpLists into a single list. */
  private def mergeLists(ess: List[ExpList]): ExpList = {
    assert(ess.nonEmpty)
    if(ess.length == 1) ess.head
    else{
      val k = ess.length/2; val ess1 = ess.take(k); val ess2 = ess.drop(k)
      merge(mergeLists(ess1), mergeLists(ess2))
    }
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
    val subs2 = subs1.filter(_.nonEmpty).distinct
    val allExprs = subs2.flatMap(allExpressions)
    // println(allExprs.size); println(allExprs.mkString("\n"))
    allExprs.minBy(e => Math.abs(e.value-target))
  }
  
  def main(args: Array[String]) = {
    val (nums, target) =
      if(args.isEmpty) pickNumbers
      else (args.init.map(_.toInt).toList.sorted, args.last.toInt)
    println(nums, target)
    val start = System.nanoTime
    val e = solve(nums, target)
    println(e.toString+" = "+e.value)
    println("Time taken: "+(System.nanoTime-start)/1000000+"ms")
  }
}
