package thinkingAboutPrograms.util

import scala.reflect.{ClassTag, classTag}
import scala.language.implicitConversions

object MyTest{
  /** Class representing the argument of a test. */
  trait TestResult{ }

  /** Result of an assertion that gave true. */
  case object PassTest extends TestResult

  /** Result of an assertion that gave false. */
  case object FailTest extends TestResult

  /** Failed test, explained by message. */
  case class FailMessageTest(message: String) extends TestResult
 
  /** Check that test holds, giving a result suitable for inclusion in a test.
    * The implicit conversion allows boolean tests to be used directly in
    * tests.*/
  implicit def assert(test: Boolean): TestResult = 
    if(test) PassTest else FailTest

  /** Check that test holds, giving a result suitable for inclusion in a test;
    * in the case of failure, a FailMessagTest including message is given.
    * The implicit conversion allows boolean tests to be used directly in
    * tests.*/
  implicit def assert(test: Boolean, message: String): TestResult =
    if(test) PassTest else FailMessageTest(message)

  /** Allow assert to be used with tests that directly give a TestResult. */
  def assert(test: TestResult): TestResult = test 

  /** Check that an expression gives an exception of type E. */
  def assertThrows[E <: Throwable: ClassTag](test: => Any): TestResult = try{
    test; FailTest
  } catch { case e if classTag[E].runtimeClass.isInstance(e) => PassTest }

  // Set colours to indicate failure, success or exception from test. 
  private val Fail = Console.RED
  private val Ok = Console.BLUE
  private val Exception = Console.MAGENTA

  /** Run a test called name, with value result.
    * @param quiet should results about passing tests be omitted? */ 
  def test(name: String, quiet: Boolean = false)(result: => TestResult) = try{ 
    result match{
      case PassTest => if(!quiet) println(Ok+name+" passes") 
      case FailTest => println(Fail+name+" fails")
      case FailMessageTest(message) => println(Fail+name+" fails: "+message)
    }
  } catch {
    case e: Exception => println(Exception+name+" throws "+e.toString)
  }  

  /** Run a test called name, with value result, producing a message only if the
    * test fails or throws an exception. */
  def quietTest(name: String)(result: => TestResult) = test(name, true)(result)

  /** In order to apply the === operator, we cast values to the following
    * type. */
  case class Wrap[A](a: A){
    def === (that: Wrap[A]) = { 
      if(a == that.a) PassTest 
      else FailMessageTest(a.toString+" does not equal "+that.a.toString) 
    }
  }

  /** Implicit conversion to Wrap[A]. */
  implicit def wrap[A](a: A) = Wrap(a)

  /** Make a custom operator.  The operator produced takes an argument a that is
    * expected to pass the test given by check; if not, message(a) gives a
    * suitable error message. */
  def makeCustom[A](check: A => Boolean, message: A => String) = {
    (a: A) => if(check(a)) PassTest else FailMessageTest(message(a))
  }

  /** Make a custom operator.  The operator produced takes two arguments a and b
    * that are expected to pass the test given by compare; if not,
    * message(a,b) gives a suitable error message. */
  def makeCustom[A,B](compare: (A,B) => Boolean, message: (A,B) => String) = {
    (a: A, b: B) => if(compare(a,b)) PassTest else FailMessageTest(message(a,b))
  }
}
