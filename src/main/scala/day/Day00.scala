package day

import run.{DayInput, DayRunner, IDay, Result}

class Day00 extends IDay(0) {
  override def execute(input: DayInput): Result = {
    val result: Iterable[String] = input.lines
    Result(result.head, result.tail.head)
  }

  val test = Result.Test("Foo", Some("Bar"), "Foo\nBar")
}

object Day00 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day00()).run()
  }
}
