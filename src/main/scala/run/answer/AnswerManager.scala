package run.answer

import run.answer.AnswerManager.{IncorrectAnswer, IncorrectReason, PartAnswers}
import run.answer.AnswerSubmitter.AnswerPart
import run.answer.AnswerSubmitter.AnswerPart.{Part1, Part2}
import run.answer.CorrectnessResponse.*
import run.answer.SubmissionResponse.{AlreadySubmitted, Correct, Incorrect, Wait}
import run.{IDay, Result}
import upickle.ReadWriter

import java.nio.file.{Files, Path}

case class AnswerManager(day: Int, dayString: String) {
  def checkAndUpdateAnswers(result: Result): Unit = {
    val existingAnswers = readAnswers()
    existingAnswers.foreach(answers =>
      checkAnswerForPart(1, result.part1, answers.part1)
      checkAnswerForPart(2, result.part2, answers.part2)
    )
    var updatedAnswers = existingAnswers.getOrElse(Answers.EMPTY)
    if (result.part1 != IDay.incomplete && updatedAnswers.part1.correct.isEmpty) {
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
    private def updatePart(
      response: CorrectnessResponse,
      result: Any,
      part: PartAnswers,
      answerFactory: PartAnswers => Answers,
      answerPart: AnswerPart,
    ): Answers = {
      val s = result.toString
      response match {
        case YES => answerFactory(part.withCorrect(s))
        case NO => answerFactory(part.withAddedIncorrect(IncorrectAnswer(s, IncorrectReason.WRONG)))
        case TOO_HIGH => answerFactory(part.withAddedIncorrect(IncorrectAnswer(s, IncorrectReason.TOO_HIGH)))
        case TOO_LOW => answerFactory(part.withAddedIncorrect(IncorrectAnswer(s, IncorrectReason.TOO_LOW)))
        case SUBMIT =>
          val submitter = AnswerSubmitter(2016, day)
          submitter.submit(answerPart, s) match {
            case Left(body) => throw new Exception(s"Unexpected response when submitting answer:\n$body")
            case Right(response: SubmissionResponse) => response match {
              case Correct =>
                println("⭐ Submitted the correct answer!" + (if (answerPart == Part1) " Please refresh the question page to see part 2." else ""))
                answerFactory(part.withCorrect(s))
              case Incorrect(reason, submitTime, waitDuration) =>
                println(s"The submitted answer was ${reason.pretty}. Please wait $waitDuration before submitting another answer.")
                answerFactory(part.withAddedIncorrect(IncorrectAnswer(s, reason)))
              case AlreadySubmitted =>
                println("The answer for that part has already been submitted.")
                this
              case Wait(submitTime, waitDuration) =>
                println(s"Please wait $waitDuration before submitting another answer.")
                this
            }
          }
      }
    }

    def updatePart1(response: CorrectnessResponse, result: Any): Answers = {
      updatePart(response, result, part1, part => copy(part1 = part), Part1)
    }

    def updatePart2(response: CorrectnessResponse, result: Any): Answers = {
      updatePart(response, result, part2, part => copy(part2 = part), Part2)
    }
  }

  private object Answers {
    val EMPTY = Answers(PartAnswers.EMPTY, PartAnswers.EMPTY)
  }
}

object AnswerManager {
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

  enum IncorrectReason(val pretty: String) derives ReadWriter {
    case WRONG extends IncorrectReason("too high")
    case TOO_HIGH extends IncorrectReason("too low")
    case TOO_LOW extends IncorrectReason("wrong")
  }
}
