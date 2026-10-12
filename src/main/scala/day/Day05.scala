package day

import run.{DayInput, DayRunner, IDay, Result}
import utility.*

import java.security.MessageDigest
import scala.annotation.tailrec

class Day05 extends IDay(5) {
  override def execute(input: DayInput): Result = {
    val exampleMd5 = md5Hex("abc3231929")
    assert(isValid(exampleMd5), s"expected $exampleMd5 to start with five zeroes")

    val doorId = input.contents.trim
    Result(part1(doorId), part2(doorId))
  }

  private def part1(doorId: String) = {
    LazyList.from(0)
      .map(i => s"$doorId$i")
      .map(md5Hex)
      .filter(isValid)
      .take(8)
      .map(_(5))
      .mkString
      .toLowerCase
  }

  private def part2(doorId: String) = {
    @tailrec
    def rec(n: Int, result: Vector[Char], filled: Int): String = {
      if (filled == 8) {
        result.mkString.toLowerCase
      } else {
        val hash = md5Hex(s"$doorId$n")
        if (isValid(hash)) {
          val pos = hash(5) - '0'
          if (pos >= 0 && pos <= 7) {
            val c = hash(6)
            if (result(pos) == '_') {
              return rec(n + 1, result.updated(pos, c), filled + 1)
            }
          }
        }
        rec(n + 1, result, filled)
      }
    }

    rec(0, Vector.from("_".repeat(8)), 0)
  }

  private def isValid(hash: String): Boolean = hash.startsWith("00000")

  private val md5Digest = MessageDigest.getInstance("MD5")

  private def md5Hex(s: String): String = {
    md5Digest.digest(s.getBytes).map(_.toHexString).mkString
  }

  override val test: List[Result.Test] = List(
    Result.Test(Some("18f47a30"), Some("05ace8e3"), "abc")
  )
}

object Day05 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day05()).run()
  }
}
