package day

import run.{DayInput, DayRunner, IDay, Result}

class Day06 extends IDay(6) {
  override def execute(input: DayInput): Result = {
    val messages = input.lines
    Result(part1(messages), part2(messages))
  }

  private def part1(messages: Iterable[String]) = {
    characterFrequenciesByColumn(messages)
      .map(_.maxBy(_._2))
      .map(_._1)
      .mkString
  }

  private def part2(messages: Iterable[String]) = {
    characterFrequenciesByColumn(messages)
      .map(_.minBy(_._2))
      .map(_._1)
      .mkString
  }

  private def characterFrequenciesByColumn(messages: Iterable[String]): IndexedSeq[Map[Char, Int]] = {
    Range(0, messages.head.length)
      .map(col => messages.map(_(col)))
      .map(chars => chars.toSet.map(c => c -> chars.count(_ == c)).toMap)
  }

  override val test: List[Result.Test] = List(
    Result.Test(Some("easter"), Some("advent"), "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar")
  )
}

object Day06 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day06()).run()
  }
}
