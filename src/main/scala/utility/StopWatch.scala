package utility

import java.time.{Duration, Instant}

/**
 * Create a new running stop watch using [[StopWatch.start]], then stop with [[Running.stop]],
 * and get duration with [[Finished.duration]].
 */
object StopWatch {
  def start: Running = Running(Instant.now())

  case class Running(start: Instant) {
    def stop: Finished = Finished(start, Instant.now())
  }

  case class Finished(start: Instant, end: Instant) {
    val duration: Duration = Duration.between(start, end)

    val millis: Long = duration.toMillis
  }
}
