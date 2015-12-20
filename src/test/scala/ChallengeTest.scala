import org.scalatest.FunSuite

class ChallengeTest extends FunSuite {

  test("Average Waiting time for 0 customers is not a valid request") {
    intercept[IllegalArgumentException] {
      MinimumAverageWatingTimeCalculator calculate(0, List())
    }
  }

  test("List of ordering and cooking time should have the same size like customers or produce IllegalArgumentException") {
    intercept[IllegalArgumentException] {
      (MinimumAverageWatingTimeCalculator calculate(1, List()))
    }
  }

  test("waiting time for just one customer should be just the processing time") {
    val times = List((5,4))
    assert((MinimumAverageWatingTimeCalculator calculate(1, times)) == 4)
  }

  test("test case 1: ((0,3),(1,9),(2,6)) -> 9") {
    val times = List((0,3),(1,9),(2,6))
    assert((MinimumAverageWatingTimeCalculator calculate(3, times)) == 9)
  }

  test("test case 2: ((0,3),(1,9),(2,5)) -> 8") {
    val times = List((0,3),(1,9),(2,5))
    assert((MinimumAverageWatingTimeCalculator calculate(3, times)) == 8)
  }

}