import utility.StopWatch

object TestRunnerAll {
  def main(args: Array[String]): Unit = {
    val stopWatch = StopWatch.start
    TestRunner.dayRunners.foreach(TestRunner.runDay)
    val executionTimeMs: Long = stopWatch.stop.millis
    println(s"Total execution time: ${executionTimeMs}ms")
  }
}
