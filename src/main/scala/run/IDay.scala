package run

import run.{DayInput, Result}

trait IDay(val dayNumber: Int) {
  def execute(input: DayInput): Result

  def test: Result.Test | List[Result.Test]
}

object IDay {
  val incomplete: String = "INCOMPLETE"
}
