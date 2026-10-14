package day

import run.{DayInput, DayRunner, IDay, Result}
import utility.*

class Day08 extends IDay(8) {
  override def execute(input: DayInput): Result = {
    val instructions = input.readLines(readInstruction)
    val startWidth = 50
    val startHeight = 6
    val startingScreen = Screen(Grid2D.fill(startHeight, startWidth)(false))
    val finalScreen = instructions.foldLeft(startingScreen)((screen, instruction) => instruction.apply(screen))
    Result(finalScreen.litCount, finalScreen)
  }

  private case class Screen(state: Grid2D[Boolean]) {
    val width: Int = state.colCount
    val height: Int = state.rowCount

    lazy val litCount: Int = state.count(b => b)

    override def toString: String = s"\n${state.toGridString(b => if (b) '#' else '.')}\n"
  }

  private trait Instruction extends Function[Screen, Screen]

  private case class Rect(width: Int, height: Int) extends Instruction {
    override def apply(screen: Screen): Screen = {
      Screen(screen.state.zipWithIndex.map((b, pos) =>
        b || pos.x < width && pos.y < height
      ))
    }
  }

  private case class RotateRow(y: Int, by: Int) extends Instruction {
    def apply(screen: Screen): Screen = {
      Screen(screen.state.zipWithIndex.map((b, pos) =>
        if (pos.y != y) b else screen.state.at(Point2D((pos.x - by).positiveModulus(screen.width), y))
      ))
    }
  }

  private case class RotateCol(x: Int, by: Int) extends Instruction {
    def apply(screen: Screen): Screen = {
      Screen(screen.state.zipWithIndex.map((b, pos) =>
        if (pos.x != x) b else screen.state.at(Point2D(x, (pos.y - by).positiveModulus(screen.height)))
      ))
    }
  }

  private val RectRe = "rect (\\d+)x(\\d+)".r
  private val RotateColRe = "rotate column x=(\\d+) by (\\d+)".r
  private val RotateRowRe = "rotate row y=(\\d+) by (\\d+)".r

  private def readInstruction(line: String): Instruction = line match {
    case RectRe(w, h) => Rect(w.toInt, h.toInt)
    case RotateColRe(x, n) => RotateCol(x.toInt, n.toInt)
    case RotateRowRe(y, n) => RotateRow(y.toInt, n.toInt)
  }

  override val test: List[Result.Test] = List(
//    Result.Test(None, None, "rect 3x2\nrotate column x=1 by 1\nrotate row y=0 by 4\nrotate column x=1 by 1")
  )
}

object Day08 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day08()).run()
  }
}
