package run

import utility.Grid2D

import scala.reflect.ClassTag
import scala.util.matching.Regex

case class DayInput(contents: String) {
  def chars: LazyList[Char] = LazyList.from(contents)

  def lines: LazyList[String] = LazyList.from(contents.linesIterator)

  def readLines[A](f: String => A): LazyList[A] = lines.map(f)

  def readLinesRegex[A](regex: Regex, f: Regex.Match => A): LazyList[A] = readLines(line => {
    regex.findFirstMatchIn(line) match {
      case Some(m) => f.apply(m)
      case None => throw IllegalArgumentException(s"Unable to parse line: $line")
    }
  })

  def readGrid2D[A: ClassTag](f: Char => A): Grid2D[A] = Grid2D.from2DCharArray(contents, f)

  def takeWhile(cond: String => Boolean): DayInput = {
    DayInput(lines.takeWhile(cond).mkString("\n").trim)
  }

  def dropWhile(cond: String => Boolean): DayInput = {
    DayInput(lines.dropWhile(cond).mkString("\n").trim)
  }

  def findFirstMatch[A](regex: Regex, f: Regex.Match => A): A = {
    regex.findFirstMatchIn(contents) match {
      case Some(m) => f.apply(m)
      case None => throw IllegalArgumentException(s"Cannot find match for $regex")
    }
  }
}
