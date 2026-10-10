package run

case class Result(part1: Any, part2: Any) {
  override def toString: String = s"Part 1: $part1, Part 2: $part2"
}

object Result {
  case class Test(expected1: Any, expected2: Option[Any], input: String)
}
