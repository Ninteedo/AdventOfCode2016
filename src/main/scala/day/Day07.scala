package day

import run.{DayInput, DayRunner, IDay, Result}
import utility.*

class Day07 extends IDay(7) {
  override def execute(input: DayInput): Result = {
    assert(isAbba("abba"))
    assert(isAbba("ioxxoj"))
    assert(!isAbba("mnop"))
    assert(!isAbba("aaaa"))

    assert(isTls("abba[mnop]qrst"))
    assert(isTls("ioxxoj[asdfgh]zxcvbn"))
    assert(!isTls("abcd[bddb]xyyx"))
    assert(!isTls("aaaa[qwer]tyui"))

    assert(isSsl("aba[bab]xyz"))
    assert(!isSsl("xyx[xyx]xyx"))
    assert(isSsl("aaa[kek]eke"))
    assert(isSsl("zazbz[bzb]cdb"))

    val addresses = input.lines
    Result(part1(addresses), part2(addresses))
  }

  private def part1(addresses: Seq[String]): Int = {
    addresses.count(isTls)
  }

  private def part2(addresses: Seq[String]): Int = {
    addresses.count(isSsl)
  }

  private def isAbba(section: String): Boolean = {
    section.sliding(4).filter(_.length == 4).exists(s => s(0) == s(3) && s(1) == s(2) && s(0) != s(1))
  }

  private def isTls(address: String): Boolean = {
    val abbas = address.split("[\\[\\]]")
      .map(isAbba)
      .zipWithIndex
    abbas.exists(_._1) && abbas.forall((b, i) => !b || i.isEven)
  }

  private def getAbas(sections: Iterable[String]): Set[String] = {
    sections.flatMap(_.sliding(3).filter(_.length == 3).filter(s => s(0) == s(2) && s(0) != s(1))).toSet
  }

  private def isSsl(address: String): Boolean = {
    val sections = address.split("[\\[\\]]").zipWithIndex
    val baseSections = sections.filter(_._2.isEven).map(_._1)
    val hyperSections = sections.filter(_._2.isOdd).map(_._1)

    val abaPairs = getAbas(baseSections).map(s => (s(0), s(1)))
    val babPairs = getAbas(hyperSections).map(s => (s(1), s(0)))
    abaPairs.intersect(babPairs).nonEmpty
  }

  override val test: List[Result.Test] = List(
    Result.Test(Some(2), None, "abba[mnop]qrst\nabcd[bddb]xyyx\naaaa[qwer]tyui\nioxxoj[asdfgh]zxcvbn")
  )
}

object Day07 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day07()).run()
  }
}
