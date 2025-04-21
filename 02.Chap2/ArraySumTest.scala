package thinkingAboutPrograms.arraySum

import ArraySum.findSum
import thinkingAboutPrograms.util.MyTest._

/** Some simple tests on the array sum program. */
object ArraySumTest{
  def main(args: Array[String]) = {
    test("empty"){ findSum(Array()) == 0 }
    test("singleton"){ findSum(Array(3)) == 3 }
    test("longer"){ findSum(Array(3, 1, 4, 0, 3)) == 11 }
    test("negatives"){ findSum(Array(-4, -2, -7)) == -13 }
  }
}
