import run.{DayInput, DayRunner}
import utility.StopWatch

import java.time.Duration

object Benchmark {
  def main(args: Array[String]): Unit = {
    val stopWatch = StopWatch.start
    val results = TestRunner.dayRunners.filter(_.dayNumber > 0).map(day => {
      val runner = DayRunner(day)
      val input = runner.readInput
      val stopWatch = StopWatch.start
      val result = day.execute(DayInput(input))
      val executionDuration = stopWatch.stop.duration
      (day.dayNumber, executionDuration)
    }).toList
    val executionTimeMs: Long = stopWatch.stop.millis
    println(s"${formatResultsTable(results)}\nTotal execution time: ${executionTimeMs}ms")
  }

  def formatResultsTable(results: List[(Int, Duration)]): String = {
    val headers = ("Day", "Run Time")
    val rows = results.map(p => (f"Day ${p._1}%02d", s"${p._2.toMillis}ms"))
    val columnWidths = (math.max(headers._1.length, rows.map(_._1.length).max), math.max(headers._2.length, rows.map(_._2.length).max))

    def formatLine(row: (String, String), noPadding: Boolean = false): String = {
      def formatColumn(contents: String, columnWidth: Int) = {
        val res = s"$contents${" ".repeat(math.max(0, columnWidth - contents.length))}"
        if (noPadding) res else s" $res "
      }

      s"|${formatColumn(row._1, columnWidths._1)}|${formatColumn(row._2, columnWidths._2)}|"
    }

    val lines = formatLine(headers) :: formatLine(("-".repeat(columnWidths._1 + 2), "-".repeat(columnWidths._2 + 2)), noPadding = true) :: rows.map(row => formatLine(row))
    lines.mkString("\n")
  }
}
