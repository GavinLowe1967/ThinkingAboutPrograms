package thinkingAboutPrograms.exponentiation

import scala.util.Random
import thinkingAboutPrograms.util.MyTest._

/** Some tests of the exponentiation functions. */
object ExpTest{
  /** Some simple tests of both algorithms. */
  def simpleTests = {
    test("slow 0"){ SlowExp.exp(3.0, 0) == 1.0 }
    test("slow 1"){ SlowExp.exp(3.0, 1) == 3.0 }
    test("slow 3"){ SlowExp.exp(3.0, 3) == 27.0 }    
    test("slow -1"){ 
      assertThrows[IllegalArgumentException](SlowExp.exp(3.0, -1)) }
    test("fast 0"){ FastExp.exp(3.0, 0) == 1.0 }
    test("fast 1"){ FastExp.exp(3.0, 1) == 3.0 }
    test("fast 3"){ FastExp.exp(3.0, 3) == 27.0 }
    test("fast -1"){ 
      assertThrows[IllegalArgumentException](FastExp.exp(3.0, -1)) }
  }

  /** Are y and z approximately equal? */
  def approx(y: Double, z: Double) = 
    assert(y != 0.0 && Math.abs((y-z)/y) < 1.0E-12 || Math.abs(y-z) < 1.0E-20,
      y.toString+" is not approximately equal to "+z)

  /** Tests that choose random values for x and n, run both algorithms, and
    * compare the results. */
  def randomTests = {
    for(i <- 0 until 10000000){
      val x = Random.nextDouble()*20.0-10.0
      val n = Random.nextInt(300)
      if(false) 
        // Followng version fails because of rounding errors
        quietTest(x.toString+" ^ "+n){ SlowExp.exp(x,n) === FastExp.exp(x,n) }
      else quietTest(x.toString+" ^ "+n){ 
        approx(SlowExp.exp(x,n), FastExp.exp(x,n)) }
      if(i%100000 == 0) print(".")
    }
    println()
  }

  def main(args: Array[String]) = { simpleTests; randomTests }
}
