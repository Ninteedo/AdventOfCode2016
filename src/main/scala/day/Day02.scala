package day

import run.{DayInput, DayRunner, IDay, Result}
import utility.Direction.{Down, Left, Right, Up}
import utility.{Direction, Grid2D, Point2D}

import scala.annotation.tailrec

class Day02 extends IDay(2) {
  type Instructions = List[List[Direction]]

  private val startButton = 5

  override def execute(input: DayInput): Result = {
    val instructions = input.readLines(_.map(readDirection).toList).toList
    Result(part1(instructions), part2(instructions))
  }

  private def part1(instructions: Instructions) = {
    val keypad = Keypad(Grid2D.from2DCharArray("123\n456\n789", _.asDigit))

    applyInstructions(keypad, n => (n + '0').toChar)(instructions, keypad.startPosition, "")
  }

  private def part2(instructions: Instructions) = {
    def charToInt: Char => Int = c => if (c.isDigit) c.asDigit else if (c == ' ') -1 else c.toInt - 'A' + 10
    def intToChar: Int => Char = n => if (n < 10) (n + '0').toChar else (n + 'A' - 10).toChar
    val keypad = Keypad(Grid2D.from2DCharArray("  1  \n 234 \n56789\n ABC \n  D  ", charToInt))

    applyInstructions(keypad, intToChar)(instructions, keypad.startPosition, "")
  }

  @tailrec
  private def applyInstructions(keypad: Keypad, intToChar: Int => Char)(instructions: Instructions, current: Point2D, result: String): String = {
    instructions match {
      case Nil => result
      case head :: next =>
        val newPosition = keypad.applyMoves(head, current)
        applyInstructions(keypad, intToChar)(next, newPosition, result + intToChar(keypad.get(newPosition)))
    }
  }

  private case class Keypad(grid: Grid2D[Int]) {
    @tailrec
    final def applyMoves(directions: List[Direction], current: Point2D): Point2D = {
      directions match {
        case Nil => current
        case head :: next =>
          val moved = current + head
          val nextPosition = if (grid.contains(moved) && grid.at(moved) >= 0) moved else current
          applyMoves(next, nextPosition)
      }
    }

    def get(pos: Point2D): Int = grid.at(pos)

    val startPosition: Point2D = grid.indexWhere(_ == startButton).get
  }

  def readDirection(c: Char): Direction = c match {
    case 'U' => Up
    case 'R' => Right
    case 'D' => Down
    case 'L' => Left
    case _   => throw new IllegalArgumentException(s"Unexpected direction '$c'")
  }

  override val test: List[Result.Test] = List(
    Result.Test(Some("1985"), Some("5DB3"), "ULL\nRRDDD\nLURDL\nUUUUD")
  )
}

object Day02 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day02()).run()
  }
}
