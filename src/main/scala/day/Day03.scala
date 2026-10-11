package day

import run.{DayInput, DayRunner, IDay, Result}

//noinspection ZeroIndexToHead
class Day03 extends IDay(3) {
  override def execute(input: DayInput): Result = {
    val rows = input.readLines(_.trim.split(' ').filterNot(_.isBlank).map(_.toInt).toList).toList
    Result(part1(rows), part2(rows))
  }

  private def part1(rows: List[List[Int]]) = {
    val triangles = rows.map(row => Triangle(row(0), row(1), row(2)))
    triangles.count(_.isValid)
  }

  private def part2(rows: List[List[Int]]) = {
    val cols = Range(0, 3).map(col => rows.map(_(col)))
    assert(cols.forall(_.length % 3 == 0), "column lengths must be divisible by 3")
    val triangles = cols.flatMap(column => column.grouped(3).map(group => Triangle(group(0), group(1), group(2))))
    triangles.count(_.isValid)
  }

  private case class Triangle(side1: Int, side2: Int, side3: Int) {
    val isValid: Boolean = side2 + side3 > side1 && side1 + side3 > side2 && side1 + side2 > side3
  }

  override val test: List[Result.Test] = List(
    Result.Test(Some(1), Some(2), "5 10 25\n5 5 5\n 5 10 15")
  )
}

object Day03 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day03()).run()
  }
}
