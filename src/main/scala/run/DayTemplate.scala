package run

import run.IDay
import run.DayInput
import run.IDay.incomplete
import utility.*

class DayTemplate extends IDay(-1) {
  override def execute(input: DayInput): Result = {
    Result(part1(), part2())
  }

  private def part1() = {
    incomplete
  }

  private def part2() = {
    incomplete
  }

  override val test: List[Result.Test] = List()
}

object DayTemplate {
  def main(args: Array[String]): Unit = {
    DayRunner(DayTemplate()).run()
  }
}
