package day

import run.{DayInput, DayRunner, IDay, Result}

class Day04 extends IDay(4) {
  override def execute(input: DayInput): Result = {
    val entries = input.readLinesRegex(
      "([a-z-]+)-(\\d+)\\[([a-z]+)]".r,
      m => Entry(m.group(1), m.group(2).toInt, m.group(3))
    ).toList

    Result(part1(entries), part2(entries))
  }

  private def part1(entries: List[Entry]) = {
    entries.filter(_.isReal).map(_.sectorId).sum
  }

  private def part2(entries: List[Entry]) = {
    entries.filter(_.isReal).find(_.shiftedName.contains("northpole")).get.sectorId
  }

  private val charCountOrdering: Ordering[(Char, Int)] = Ordering.by[(Char, Int), Int](_._2).reverse.orElseBy(_._1)

  private case class Entry(encryptedName: String, sectorId: Int, checksum: String) {
    val isReal: Boolean = {
      val charSet = encryptedName.filter(_ != '-').toSet
      val charCounts = charSet.map(c => c -> encryptedName.count(_ == c)).toList
      val permittedChars = charCounts.sorted(using charCountOrdering).take(5).map(_._1)
      checksum.forall(permittedChars.contains)
    }

    lazy val shiftedName: String = {
      encryptedName.map(c => if (c == '-') ' ' else (((c - 'a' + sectorId) % 26) + 'a').toChar).mkString
    }
  }

  override val test: List[Result.Test] = List(
//    Result.Test(Some(1514), None, "aaaaa-bbb-z-y-x-123[abxyz]\na-b-c-d-e-f-g-h-987[abcde]\nnot-a-real-room-404[oarel]\ntotally-real-room-200[decoy]")
  )
}

object Day04 {
  def main(args: Array[String]): Unit = {
    DayRunner(Day04()).run()
  }
}
