package day

import run.{DayRunner, Result}
import utility.*
import run.IDay
import run.DayInput
import run.IDay.incomplete
import utility.*

import scala.annotation.tailrec

class Day09 extends IDay(9) {
  override def execute(input: DayInput): Result = {
    Result(part1(input.toString), part2(input.toString))
  }

  private val RepeatRegex = "^\\((\\d+)x(\\d+)\\).+".r

  private def part1(compressed: String) = {
    @tailrec
    def rec(remaining: String, result: Int): Int = remaining match {
      case "" => result
      case RepeatRegex(length, n) =>
        val from = 3 + length.length + n.length
        val to = from + length.toInt
        rec(remaining.substring(to), result + length.toInt * n.toInt)
      case _ => rec(remaining.tail, result + 1)
    }

    rec(compressed, 0)
  }

  private def part2(compressed: String) = {
    def rec(remaining: String): Long = remaining match {
      case "" => 0
      case RepeatRegex(length, n) =>
        val from = 3 + length.length + n.length
        val to = from + length.toInt
        n.toInt * rec(remaining.substring(from, to)) + rec(remaining.substring(to))
      case _ => 1 + rec(remaining.tail)
    }

    rec(compressed)
  }

  override val test: List[Result.Test] = List(
    Result.Test(Some(6), None, "ADVENT"),
    Result.Test(Some(7), None, "A(1x5)BC"),
    Result.Test(Some(9), Some(9), "(3x3)XYZ"),
    Result.Test(Some(11), None, "A(2x2)BCD(2x2)EFG"),
    Result.Test(Some(6), None, "(6x1)(1x3)A"),
    Result.Test(Some(18), Some(20), "X(8x2)(3x3)ABCY"),
    Result.Test(None, Some(241920), "(27x12)(20x12)(13x14)(7x10)(1x12)A"),
    Result.Test(None, Some(445), "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"),
  )
}

object Day09 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day09()).run()
  }
}
