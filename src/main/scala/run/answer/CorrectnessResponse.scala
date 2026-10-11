package run.answer

import scala.io.StdIn.readLine

private[run] enum CorrectnessResponse(val char: Char, val name: String) {
  case YES extends CorrectnessResponse('Y', "yes")
  case NO extends CorrectnessResponse('N', "no")
  case TOO_HIGH extends CorrectnessResponse('H', "too high")
  case TOO_LOW extends CorrectnessResponse('L', "too low")
  case SUBMIT extends CorrectnessResponse('S', "submit")
}

private[run] object CorrectnessResponse {
  def read(part: Int): CorrectnessResponse = {
    println(s"Was the answer for part $part correct? [Submit/Yes/No/too High/too Low]: ")
    val response = readLine()
    val parsed = parse(response)
    parsed.getOrElse(read(part))
  }

  private def parse(input: String): Option[CorrectnessResponse] = {
    if (input.length == 1) {
      val c = input.head.toUpper
      CorrectnessResponse.values.find(_.char == c)
    } else {
      val lower = input.toLowerCase
      CorrectnessResponse.values.find(_.name == lower)
    }
  }
}
