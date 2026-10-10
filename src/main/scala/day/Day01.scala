package day

import run.{DayInput, DayRunner, IDay, Result}
import utility.*
import utility.Direction.North

import scala.annotation.tailrec

class Day01 extends IDay(1) {
  private val startPos = Point2D.ZERO

  override def execute(input: DayInput): Result = {
    val instructions = input.contents
      .split(", ")
      .toList
      .map(_.trim)
      .map(Instruction.read)
    Result(part1(instructions), part2(instructions))
  }

  private def part1(instructions: List[Instruction]) = {
    case class State(direction: Direction, position: Point2D) {
      def applyInstruction(instruction: Instruction): State = {
        val newDirection = instruction.turn.applyTurn(direction)
        State(
          newDirection,
          position + (newDirection.toPoint * instruction.n)
        )
      }
    }

    val start = State(Direction.North, startPos)
    val endPoint = instructions.foldLeft(start)((state, instruction) => state.applyInstruction(instruction))
    endPoint.position.mannDist(startPos)
  }

  private def part2(instructions: List[Instruction]) = {
    @tailrec
    def rec(
      instructions: List[Instruction],
      direction: Direction,
      position: Point2D,
      visited: Set[Point2D]
    ): Point2D = {
      instructions.headOption match {
        case None => startPos // no duplicate found
        case Some(instruction) =>
          val newDirection = instruction.turn.applyTurn(direction)
          val allVisited = Range.inclusive(1, instruction.n).map(n => position + (newDirection.toPoint * n))
          val newPosition = allVisited.last
          allVisited.find(visited.contains) match {
            case Some(duplicateLocation) => duplicateLocation
            case None => rec(
              instructions.tail,
              newDirection,
              newPosition,
              visited ++ allVisited,
            )
          }
      }
    }

    val firstDoubleVisit = rec(instructions, North, startPos, Set.empty)
    firstDoubleVisit.mannDist(startPos)
  }

  enum Turn {
    case Left, Right

    def applyTurn(direction: Direction): Direction = this match {
      case Left => direction.rotateCounterClockwise
      case Right => direction.rotateClockwise
    }
  }

  case class Instruction(turn: Turn, n: Int)

  object Instruction {
    def read(s: String): Instruction = {
      Instruction(if (s.charAt(0) == 'R') Turn.Right else Turn.Left, s.substring(1, s.length).toInt)
    }
  }

  override val test: List[Result.Test] = List(
    Result.Test(Some(5), None, "R2, L3"),
    Result.Test(Some(2), None, "R2, R2, R2"),
    Result.Test(Some(12), None, "R5, L5, R5, R3 "),
    Result.Test(None, Some(4), "R8, R4, R4, R8"),
  )
}

object Day01 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day01()).run()
  }
}
