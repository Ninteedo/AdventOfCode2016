package run

import upickle.ReadWriter
import utility.StopWatch

import java.nio.file.{Files, Path}
import scala.io.Source
import scala.io.StdIn.readLine

case class DayRunner(day: IDay) {
  private val n = day.dayNumber
  private val dayString = f"$n%02d"

  def readInput: String = {
    val source = Source.fromFile(s"input/$dayString.txt")
    try source.mkString.trim finally source.close()
  }

  def run(skipTest: Boolean = false): Unit = {
    if (!skipTest) {
      day.test match {
        case test: Result.Test => runTestCase(test)
        case tests: List[Result.Test] => tests.foreach(runTestCase)
      }
    }
    val result = runMainCase(readInput)
    checkAndUpdateAnswers(result)
  }

  private def runTestCase(test: Result.Test): Unit = {
    val result = day.execute(DayInput(test.input))
    assert(test.expected1 == result.part1, assertionFailureMessage(1, test.expected1, result.part1))
    test.expected2.foreach(expectedB =>
      assert(expectedB == result.part2, assertionFailureMessage(2, expectedB, result.part2))
    )
  }

  private def runMainCase(input: String): Result = {
    val stopWatch = StopWatch.start
    val result = day.execute(DayInput(input))
    val executionMs = stopWatch.stop.millis
    println(s"Day $dayString: (Part 1: ${result.part1}, Part 2: ${result.part2})  [${executionMs}ms]")
    result
  }

  private def checkAndUpdateAnswers(result: Result): Unit = {
    val existingAnswers = readAnswers()
    existingAnswers.foreach(answers =>
      checkAnswerForPart(1, result.part1, answers.part1)
      checkAnswerForPart(2, result.part2, answers.part2)
    )
    var updatedAnswers = existingAnswers.getOrElse(Answers.EMPTY)
    if (updatedAnswers.part1.correct.isEmpty) {
      updatedAnswers = updatedAnswers.updatePart1(CorrectnessResponse.read(1), result.part1)
    }
    if (result.part2 != IDay.incomplete && updatedAnswers.part2.correct.isEmpty) {
      updatedAnswers = updatedAnswers.updatePart2(CorrectnessResponse.read(2), result.part2)
    }
    writeAnswers(updatedAnswers)
  }

  private def checkAnswerForPart(part: Int, answer: Any, pastAnswers: PartAnswers): Unit = {
    pastAnswers.correct match {
      case Some(correct) => assert(answer.toString == correct, s"Part $part answer did not match the past correct answer.\nPast (correct): $correct\nAnswer: $answer")
      case None => pastAnswers.incorrect.foreach(incorrect => incorrect.reason match {
        case IncorrectReason.WRONG =>
          assert(answer.toString != incorrect.value, s"Part $part answer matched a past incorrect answer.\nAnswer: $answer")
        case IncorrectReason.TOO_HIGH =>
          assert(answer.toString.toLong < incorrect.value.toLong, s"Part $part answer was not lower than a past answer that was too high.\nPast (too high): ${incorrect.value}\nAnswer: $answer")
        case IncorrectReason.TOO_LOW =>
          assert(answer.toString.toLong > incorrect.value.toLong, s"Part $part answer was not higher than a past answer that was too low.\nPast (too low): ${incorrect.value}\nAnswer: $answer")
      })
    }
  }

  private def assertionFailureMessage(part: Int, expected: Any, actual: Any): String = {
    s"Part $part expected result did not match actual result.\nExpected: $expected\nActual: $actual"
  }

  private val answersPath = Path.of(s"answers/$dayString.json")

  private def readAnswers(): Option[Answers] = {
    if (!Files.exists(answersPath)) {
      None
    } else {
      val contents = Files.readString(answersPath)
      Some(upickle.read[Answers](contents))
    }
  }

  private def writeAnswers(answers: Answers): Unit = {
    Files.createDirectories(answersPath.getParent)
    Files.writeString(answersPath, upickle.write(answers))
  }

  private case class Answers(part1: PartAnswers, part2: PartAnswers) derives ReadWriter {
    def updatePart1(response: CorrectnessResponse, result: Any): Answers = response match {
      case CorrectnessResponse.YES => copy(part1 = part1.withCorrect(result.toString))
      case CorrectnessResponse.NO => copy(part1 = part1.withAddedIncorrect(IncorrectAnswer(result.toString, IncorrectReason.WRONG)))
      case CorrectnessResponse.TOO_HIGH => copy(part1 = part1.withAddedIncorrect(IncorrectAnswer(result.toString, IncorrectReason.TOO_HIGH)))
      case CorrectnessResponse.TOO_LOW => copy(part1 = part1.withAddedIncorrect(IncorrectAnswer(result.toString, IncorrectReason.TOO_LOW)))
    }

    def updatePart2(response: CorrectnessResponse, result: Any): Answers = response match {
      case CorrectnessResponse.YES => copy(part2 = part2.withCorrect(result.toString))
      case CorrectnessResponse.NO => copy(part2 = part2.withAddedIncorrect(IncorrectAnswer(result.toString, IncorrectReason.WRONG)))
      case CorrectnessResponse.TOO_HIGH => copy(part2 = part2.withAddedIncorrect(IncorrectAnswer(result.toString, IncorrectReason.TOO_HIGH)))
      case CorrectnessResponse.TOO_LOW => copy(part2 = part2.withAddedIncorrect(IncorrectAnswer(result.toString, IncorrectReason.TOO_LOW)))
    }
  }

  private object Answers {
    val EMPTY = Answers(PartAnswers.EMPTY, PartAnswers.EMPTY)
  }

  private case class PartAnswers(correct: Option[String], incorrect: List[IncorrectAnswer]) derives ReadWriter {
    def withCorrect(newCorrect: String): PartAnswers = {
      copy(correct = Some(newCorrect))
    }

    def withAddedIncorrect(incorrectAnswer: IncorrectAnswer): PartAnswers = {
      copy(incorrect = incorrect :+ incorrectAnswer)
    }
  }

  private object PartAnswers {
    val EMPTY = PartAnswers(None, Nil)
  }

  private case class IncorrectAnswer(value: String, reason: IncorrectReason) derives ReadWriter

  private enum IncorrectReason derives ReadWriter {
    case WRONG, TOO_HIGH, TOO_LOW
  }

  private enum CorrectnessResponse(val char: Char, val name: String) {
    case YES extends CorrectnessResponse('Y', "yes")
    case NO extends CorrectnessResponse('N', "no")
    case TOO_HIGH extends CorrectnessResponse('H', "too high")
    case TOO_LOW extends CorrectnessResponse('L', "too low")
  }

  private object CorrectnessResponse {
    def read(part: Int): CorrectnessResponse = {
      println(s"Was the answer for part $part correct? [Yes/No/too High/too Low]: ")
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
}
