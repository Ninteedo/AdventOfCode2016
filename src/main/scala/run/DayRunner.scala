package run

import utility.StopWatch

import scala.io.Source

case class DayRunner(day: IDay) {
  private val n = day.dayNumber
  private val dayString = f"$n%02d"
  private val answerManager = AnswerManager(n, dayString)

  def readInput: String = {
    val source = Source.fromFile(s"input/$dayString.txt")
    try source.mkString.trim finally source.close()
  }

  def run(skipTest: Boolean = false): Unit = {
    if (!skipTest) {
      day.test match {
        case test: Result.Test => runTestCase(test)
        case tests: List[Result.Test] => tests.foreach(runTestCase)
      }
    }
    val result = runMainCase(readInput)
    answerManager.checkAndUpdateAnswers(result)
  }

  private def runTestCase(test: Result.Test): Unit = {
    val result = day.execute(DayInput(test.input))
    test.expected1.foreach(expectedA =>
      assert(expectedA == result.part1, assertionFailureMessage(1, expectedA, result.part1))
    )
    test.expected2.foreach(expectedB =>
      assert(expectedB == result.part2, assertionFailureMessage(2, expectedB, result.part2))
    )
  }

  private def assertionFailureMessage(part: Int, expected: Any, actual: Any): String = {
    s"Part $part expected result did not match actual result.\nExpected: $expected\nActual: $actual"
  }

  private def runMainCase(input: String): Result = {
    val stopWatch = StopWatch.start
    val result = day.execute(DayInput(input))
    val executionMs = stopWatch.stop.millis
    println(s"Day $dayString: (Part 1: ${result.part1}, Part 2: ${result.part2})  [${executionMs}ms]")
    result
  }

}
