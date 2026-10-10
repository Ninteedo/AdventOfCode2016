package utility

case class Point3D(x: Int, y: Int, z: Int) {
  def +(other: Point3D) = Point3D(x + other.x, y + other.y, z + other.z)

  def -(other: Point3D) = Point3D(x - other.x, y - other.y, z - other.z)

  def *(factor: Int) = Point3D(x * factor, y * factor, z * factor)

  def /(divisor: Int) = Point3D(x / divisor, y / divisor, z / divisor)

  def mannDist(other: Point3D): Int = (x - other.x).abs + (y - other.y).abs + (z - other.z).abs

  def euclidDist(other: Point3D): Double = math.sqrt((x - other.x).square + (y - other.y).square + (z - other.z).square)

  def inVolume(vertex1: Point3D, vertex2: Point3D): Boolean = {
    x.inRangeInclusive(vertex1.x, vertex2.x) &&
      y.inRangeInclusive(vertex1.y, vertex2.y) &&
      z.inRangeInclusive(vertex1.z, vertex2.z)
  }

  override def toString = s"($x, $y, $z)"
}

object Point3D {
  def apply(triple: (Int, Int, Int)): Point3D = Point3D(triple._1, triple._2, triple._3)

  def readCsv(csv: String): Point3D = {
    val splits = csv.split(',')
    assert(splits.length == 3, s"Point3D requires 3 elements, not ${splits.length}")
    Point3D(splits(0).toInt, splits(1).toInt, splits(2).toInt)
  }

  val ZERO: Point3D = Point3D(0, 0, 0)

  val DIRECTIONS: LazyList[Point3D] = LazyList(xVec(1), xVec(-1), yVec(1), yVec(-1), zVec(1), zVec(-1))

  def xVec(x: Int): Point3D = Point3D(x, 0, 0)

  def yVec(y: Int): Point3D = Point3D(0, y, 0)

  def zVec(z: Int): Point3D = Point3D(0, 0, z)
}
