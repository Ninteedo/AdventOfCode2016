package run

import run.AnswerSubmitter.{AnswerPart, ResponseKind}
import sttp.client4.*
import sttp.model.Uri

import java.io.IOException
import java.nio.file.{Files, Path}

case class AnswerSubmitter(year: Int, day: Int) {

  private val uri = Uri("https", AnswerSubmitter.baseUrl, List(year.toString, "day", day.toString, "answer"))

  def submit(part: AnswerPart, answer: String): Either[String, ResponseKind] = {
    val requestData = RequestData(part, answer)
    val request = basicRequest
      .body(requestData.toMap)
      .headers(AnswerSubmitter.headers)
      .cookie("session", token)
      .post(uri)

    val backend = DefaultSyncBackend()
    val response = request.send(backend)

    response.body.flatMap(ResponseKind.fromResponseHtml)
  }

  private val cookiePath = Path.of(".cookie")

  private lazy val token = {
    try {
      Files.readString(cookiePath).trim
    } catch {
      case e: Exception => throw new IOException(s"Failed to read cookie file. Please ensure the file contains the current session cookie value. path=$cookiePath")
    }
  }

  private case class RequestData(level: String, answer: String) {
    def toMap: Map[String, String] = Map("level" -> level, "answer" -> answer)
  }

  private object RequestData {
    def apply(part: AnswerPart, answer: String): RequestData = RequestData(part.label, answer)
  }
}

object AnswerSubmitter {
  val baseUrl = "adventofcode.com"

  val headers: Map[String, String] = Map(
    "Accept-Language" -> "en-US,en;q=0.8",
    "Accept-Encoding" -> "none",
    "Accept-Charset" -> "ISO-8859-1,utf-8;q=0.7,*;q=0.3",
    "Accept" -> "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  )

  enum AnswerPart(val label: String) {
    case Part1 extends AnswerPart("1")
    case Part2 extends AnswerPart("2")
  }

  enum ResponseKind(val matchTerm: String) {
    case AnswerCorrect extends ResponseKind("That's the right answer!")
    case AnswerTooHigh extends ResponseKind("too high")
    case AnswerTooLow extends ResponseKind("too low")
    case AnswerWrong extends ResponseKind("incorrect")
    case AlreadyAnswered extends ResponseKind("already complete")
  }

  object ResponseKind {
    def fromResponseHtml(html: String): Either[String, ResponseKind] = {
      values.find(kind => html.contains(kind.matchTerm)).toRight(html)
    }
  }
}
