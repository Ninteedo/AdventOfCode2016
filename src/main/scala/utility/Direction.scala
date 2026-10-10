package utility

enum Direction {
  case North, East, South, West

  def adjustPosition(pos: Point2D): Point2D = pos + this.toPoint

  lazy val opposite: Direction = this match {
    case North => South
    case East => West
    case South => North
    case West => East
  }

  lazy val toPoint: Point2D = this match {
    case North => Point2D(0, -1)
    case East => Point2D(1, 0)
    case South => Point2D(0, 1)
    case West => Point2D(-1, 0)
  }

  lazy val rotateClockwise: Direction = this match {
    case North => East
    case East => South
    case South => West
    case West => North
  }

  lazy val rotateCounterClockwise: Direction = this match {
    case North => West
    case East => North
    case South => East
    case West => South
  }
}

object Direction {
  val fromArrow: Char => Direction = {
    case '^' => Direction.North
    case 'v' => Direction.South
    case '<' => Direction.West
    case '>' => Direction.East
    case other => throw new IllegalArgumentException(s"Invalid direction: $other")
  }
}
