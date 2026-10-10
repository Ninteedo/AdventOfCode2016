package utility

import scala.reflect.ClassTag

case class Grid2D[T: ClassTag](entries: Vector[Vector[T]]) {
  lazy val rowCount: Int = entries.length
  lazy val colCount: Int = if (rowCount > 0) entries.head.length else 0

  def at(row: Int, col: Int): T = entries(row)(col)
  def at(pos: Point2D): T = at(pos.y, pos.x)

  def atOption(row: Int, col: Int): Option[T] = if (contains(row, col)) Some(at(row, col)) else None
  def atOption(pos: Point2D): Option[T] = atOption(pos.y, pos.x)

  def getRow(row: Int): Vector[T] = entries(row)
  def getCol(col: Int): Vector[T] = entries.map(_(col))

  def headRow: Vector[T] = entries.head
  def lastRow: Vector[T] = entries.last
  def headCol: Vector[T] = getCol(0)
  def lastCol: Vector[T] = getCol(colCount - 1)

  lazy val entriesByColumn: Vector[Vector[T]] = entries.transpose

  def contains(row: Int, col: Int): Boolean = row >= 0 && col >= 0 && row < rowCount && col < colCount
  def contains(pos: Point2D): Boolean = contains(pos.y, pos.x)

  def adjacent(row1: Int, col1: Int, row2: Int, col2: Int): Boolean =
    contains(row1, col1) && contains(row2, col2) && (row1 != row2 || col1 != col2) && (row1 - row2).abs <= 1 && (col1 - col2).abs <= 1
  def adjacent(pos1: Point2D, pos2: Point2D): Boolean = adjacent(pos1.y, pos1.x, pos2.y, pos2.x)

  def points: List[Point2D] = entries.indices.flatMap(rowNum => entries(rowNum).indices.map(colNum => Point2D(colNum, rowNum))).toList
  def entriesWithPoints: List[(T, Point2D)] = entries.zipWithIndex.flatMap((rowEntries, rowNum) => rowEntries.zipWithIndex.map((entry, colNum) => (entry, Point2D(colNum, rowNum)))).toList
  def flatten: List[T] = entries.flatten.toList

  def indexWhere(f: T => Boolean): Option[Point2D] = {
    entries.zipWithIndex.find { case (row, rowNum) => row.indexWhere(f) >= 0 } match {
      case Some((row, rowNum)) => Some(Point2D(row.indexWhere(f), rowNum))
      case None => None
    }
  }

  def indicesWhere(f: T => Boolean): List[Point2D] = {
    entries.zipWithIndex.flatMap {
      case (row, rowNum) => row.indices.filter(colNum => f(at(rowNum, colNum))).map(colNum => Point2D(colNum, rowNum))
    }.toList
  }

  def transpose: Grid2D[T] = Grid2D(entriesByColumn)

  def updated(pos: Point2D, value: T): Grid2D[T] = {
    Grid2D(entries.updated(pos.y, entries(pos.y).updated(pos.x, value)))
  }

  def zipWithIndex: Grid2D[(T, Point2D)] = {
    Grid2D(entries.zipWithIndex.map((rowEntries, row) => rowEntries.zipWithIndex.map((entry, col) => (entry, Point2D(col, row)))))
  }

  def map[B: ClassTag](f: T => B): Grid2D[B] = {
    Grid2D(entries.map(_.map(f)))
  }

  def count(p: T => Boolean): Int = {
    entries.flatten.count(p)
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: Grid2D[?] => entries.zip(other.entries).forall((a, b) => a == b)
    case _ => false
  }

  lazy val hashCodeCache: Int = entries.hashCode()

  override def hashCode(): Int = hashCodeCache

  override def toString: String = entries.map(_.mkString("[", ", ", "]")).mkString("\n")

  def toGridString(f: T => Char): String = entries.map(_.map(f).mkString("")).mkString("\n")

  def toGridStringByIndex(f: Point2D => Char): String = entries.zipWithIndex.map {
    case (row, rowNum) => row.zipWithIndex.map {
      case (col, colNum) => f(Point2D(colNum, rowNum))
    }.mkString("")
  }.mkString("\n")
}

object Grid2D {
  def from2DCharArray[T: ClassTag](input: String, f: Char => T): Grid2D[T] = {
    val lines = input.split("\n")
    if (lines.nonEmpty) {
      val lineLength = lines.head.length
      assert(lines.tail.forall(_.length == lineLength), "Lines must all have same length to create 2D grid")
    }
    val entries = lines.map(line => Vector.from(line.map(f))).toVector
    Grid2D(entries)
  }
}
