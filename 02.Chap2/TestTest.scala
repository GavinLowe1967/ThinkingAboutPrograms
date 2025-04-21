package thinkingAboutPrograms.arraySum
import scala.util.Random
import thinkingAboutPrograms.util.MyTest._

/** An object to test and demonstrate the MyTest testing framework. */ 
object TestTest{
  /** Some simple tests of the testing framework. */
  def simpleTests = {
    // Below, tests beginning with "X" are expected to fail; tests beginning
    // with "E" are expected to fail with an exception; other tests are
    // expected to succeed.

    // The first three use explicit "assert"s.
    test("a"){ assert(1+1 == 2) }  // Succeeds
    test("Xa"){ assert(1+1 == 3) } // Fails
    test("Ea"){ assert(throw new RuntimeException("whatever")) } // Exception

    // The next three check for the throwing of exceptions.
    test("b"){ assertThrows[RuntimeException](1/0) } // Succeeds
    test("Eb"){ assertThrows[IllegalStateException](1/0) } // Exception
    test("Xb"){ assertThrows[RuntimeException](42) } // Fails

    // The next three use the "===" operator.
    test("c"){ 1+1 === 2 }  // Succeeds
    test("Xc"){ 1+1 === 3 } // Fails
    test("Ec"){ 1/0 === 2 } // Exception

    // The next three just take boolean expressions (without explicit
    // "assert"s).
    test("d"){ 1+1 == 2 }  // Succeeds
    test("Xd"){ 1+1 == 3 } // Fails
    test("Ed"){ 1/0 == 2 } // Exception

    // Define a custom operator to test whether its first argument is less
    // than its second.
    val lessThan = makeCustom(
      (a: Int, b: Int) => a < b,
      (a: Int, b: Int) => a.toString+" is not less than "+b.toString
    )
    // Three tests using this custom operator. 
    test("e"){ lessThan(3,4) }  // Succeeds
    test("Xe"){ lessThan(3,3) } // Fails
    test("Ee"){ lessThan(3,1/0) } // Exception

    // The next three use the "===" operator.
    test("f"){ assert(1+1 === 2) }  // Succeeds
    test("Xf"){ assert(1+1 === 3) } // Fails
    test("Ef"){ assert(1/0 === 2) } // Exception

    // Three tests using this custom operator. 
    test("g"){ assert(lessThan(3,4)) }  // Succeeds
    test("Xg"){ assert(lessThan(3,3)) } // Fails
    test("Eg"){ assert(lessThan(3,1/0)) } // Exception

    // Now three tests using quietTest
    quietTest("h"){ assert(1+1 == 2) }  // Succeeds; reports nothing
    quietTest("Xh"){ assert(1+1 == 3) } // Fails
    quietTest("Eh"){ 
      assert(throw new RuntimeException("whatever")) } // Exception
  }

  /** Tests of a sorting algorithm. */
  def sortTests = {
    // Custom check that a is a permutation of b.
    val isPerm = makeCustom(
      (a: Array[Int], b: Array[Int]) => {
        a.length == b.length && a.forall(x => a.count(_==x) == b.count(_==x))
      },
      (a: Array[Int], b: Array[Int]) => {
        a.mkString("Array(", ",", ")")+" is not a permutation of "+
        b.mkString("Array(", ",", ")")
      }
    )
    // custom check that a is sorted.
    val isSorted = makeCustom(
      (a: Array[Int]) => (0 until a.length-1).forall(i => a(i) <= a(i+1)),
      (a: Array[Int]) => a.mkString("Array(", ",", ")")+" is not sorted"
    )
    // # tests to run; size of arrays; max value in arrays
    val Reps = 200000; val Size = 40; val Max = 100
    // Now run the tests, generating random arrays, and checking that sorting
    // produced an array that is indeed sorted and a permutation of the
    // original.
    for(i <- 0 until Reps){
      val a = Array.fill(Size)(Random.nextInt(Max)) // random array
      val s = a.sorted
      // Randomly insert error into s
      // if(Random.nextInt(10000000) == 0) 
      //   s(Random.nextInt(Size)) = Random.nextInt(Max)
      quietTest("isPerm")(isPerm(a,s)); quietTest("isSorted")(isSorted(s))
      if(i%20000 == 0) print(".") // show progress
    }
    println()
  }

  def main(args: Array[String]) = {
    simpleTests
    sortTests
  }
}
