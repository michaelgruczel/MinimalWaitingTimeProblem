import org.scalatest.FunSuite

import scala.util.Random

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

  test("test with big processing times") {
    val increasingStartPoints = List.range(0, 10)
    val randomNumbers:List[Int] = Seq.fill(10)(Random.nextInt(1000000000)).toList.map(elem => Math.abs(elem))
    val testData = increasingStartPoints zip randomNumbers
    //println(testData)
    assert((MinimumAverageWatingTimeCalculator calculate(10, testData)) != 0)
  }

  test("test with a lot of customers") {
    // needed about 12 minutes on my laptop
    val increasingStartPoints = List.range(0, 100000)
    val randomNumbers:List[Int] = Seq.fill(100000)(Random.nextInt(10)).toList.map(elem => Math.abs(elem))
    val testData = increasingStartPoints zip randomNumbers
    //println(testData)
    assert((MinimumAverageWatingTimeCalculator calculate(100000, testData)) != 0)
  }

  test("test BigInt is big enough") {
    // I use BigInt in order to sum up times, so let's proove that bg in fulfills the ranges
    val maximumPossibleWaitingTime = BigInt(100000) * BigInt(1000000000) * BigInt(1000000000)
    assert(maximumPossibleWaitingTime > 0)
  }

}