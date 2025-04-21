package thinkingAboutPrograms.factorial

import Factorial.fact
import thinkingAboutPrograms.util.MyTest._

/** Some simple tests on the factorial program. */
object FactorialTest{ 
  def main(args: Array[String]) = {
    test("0"){ fact(0) === 1 }
    test("1"){ fact(1) === 1 }
    test("3"){ fact(3) === 6 }
    test("6"){ fact(6) === 720 }
    test("-1"){ assertThrows[IllegalArgumentException](fact(-1)) }
    test("-5"){ assertThrows[IllegalArgumentException](fact(-5)) }
  }
}
