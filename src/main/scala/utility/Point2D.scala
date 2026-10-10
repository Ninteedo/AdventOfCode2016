package utility

import utility.Point2D.ADJACENTS

case class Point2D(x: Int, y: Int) {
  def +(other: Point2D) = Point2D(x + other.x, y + other.y)

  def +(direction: Direction): Point2D = this + direction.toPoint

  def -(other: Point2D) = Point2D(x - other.x, y - other.y)

  def -(direction: Direction): Point2D = this - direction.toPoint

  def *(other: Point2D) = Point2D(x * other.x, y * other.y)

  def *(factor: Int) = Point2D(x * factor, y * factor)

  def /(divisor: Int) = Point2D(x / divisor, y / divisor)

  def dotProduct(other: Point2D): Int = x * other.x + y * other.y

  def mannDist(other: Point2D): Int = (x - other.x).abs + (y - other.y).abs

  def areaWith(other: Point2D): Long = ((x - other.x).abs + 1).toLong * ((y - other.y).abs + 1).toLong

  def inArea(topLeft: Point2D, bottomRight: Point2D): Boolean =
    topLeft.x <= x && x <= bottomRight.x && topLeft.y <= y && y <= bottomRight.y

  def transpose: Point2D = Point2D(y, x)

  def adjacents: LazyList[Point2D] = ADJACENTS.map(_ + this)

  override def toString = s"($x, $y)"
}

object Point2D {
  def apply(pair: (Int, Int)): Point2D = Point2D(pair._1, pair._2)

  def readCsv(csv: String): Point2D = {
    val splits = csv.split(',')
    assert(splits.length == 2, s"Point2D requires 2 elements, not ${splits.length}")
    Point2D(splits(0).toInt, splits(1).toInt)
  }

  val ZERO: Point2D = Point2D(0, 0)

  val DIRECTIONS: LazyList[Point2D] = LazyList(xVec(1), xVec(-1), yVec(1), yVec(-1))

  val ADJACENTS: LazyList[Point2D] = LazyList(
    Point2D(-1, -1), Point2D(-1, 0), Point2D(-1, 1), Point2D(0, -1),
    Point2D(0, 1), Point2D(1, -1), Point2D(1, 0), Point2D(1, 1)
  )

  def xVec(x: Int): Point2D = Point2D(x, 0)

  def yVec(y: Int): Point2D = Point2D(0, y)
}
