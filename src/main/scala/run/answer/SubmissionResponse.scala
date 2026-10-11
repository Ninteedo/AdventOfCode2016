package run.answer

import run.answer.AnswerManager.IncorrectReason

import java.time.{Duration, Instant}

sealed trait SubmissionResponse

object SubmissionResponse {
  object Correct extends SubmissionResponse

  object AlreadySubmitted extends SubmissionResponse

  case class Incorrect(reason: IncorrectReason, submitTime: Instant, waitDuration: Duration) extends SubmissionResponse {
    val noSoonerThan: Instant = submitTime.plus(waitDuration)
  }

  case class Wait(submitTime: Instant, waitDuration: Duration) extends SubmissionResponse {
    val noSoonerThan: Instant = submitTime.plus(waitDuration)
  }
}
