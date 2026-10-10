import java.io.{FileNotFoundException, InputStream}
import java.net.{HttpURLConnection, URI}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.time.{LocalDate, LocalTime}

import utility.toInt

object CreateNewDay {
  private val YEAR = 2016
  private val DAYS = 25
  private val EST_OFFSET_HOURS = 5

  def main(args: Array[String]): Unit = {
    val currentDate = LocalDate.now()
    if (currentDate.getYear <= YEAR && currentDate.getMonthValue < 12) {
      printError(s"It is not December $YEAR yet.")
      System.exit(1)
    }
    val afterDecember: Boolean = currentDate.getYear > YEAR

    val inputDirPath = Paths.get("input")
    if (!Files.exists(inputDirPath)) Files.createDirectory(inputDirPath)

    val maxDay = if (afterDecember) DAYS else
      math.min(DAYS, currentDate.getDayOfMonth - (LocalTime.now().getHour < EST_OFFSET_HOURS).toInt)
    val day = (1 to maxDay).find(day => !Files.exists(getInputPath(day)))

    day match {
      case Some(day) =>
        createDayScript(day)
        downloadInput(YEAR, day)
        System.exit(0)
      case None =>
        println(s"All input for $YEAR have been downloaded, or it's too early to download next input.")
    }
  }

  private def downloadInput(year: Int, day: Int): Unit = {
    val filePath = getInputPath(day)
    if (!Files.exists(filePath)) {
      val url = URI.create(s"https://adventofcode.com/$year/day/$day/input").toURL

      val connection = url.openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod("GET")
      val cookieFilePath = Paths.get(".cookie")
      if (!Files.exists(cookieFilePath)) {
        throw new FileNotFoundException("Could not find .cookie file for session cookie.")
      }
      val sessionCookie = Files.readString(cookieFilePath).stripSuffix("\n")
      connection.setRequestProperty("Cookie", s"session=$sessionCookie")

      var in: InputStream = null
      try {
        in = connection.getInputStream
        Files.copy(in, filePath, StandardCopyOption.REPLACE_EXISTING)
        println(s"Downloaded input for day $day.")
      } catch {
        case e: Exception =>
          printError(s"Error downloading the input file for day $day: ${e.getMessage}")
          if (e.getMessage.contains("HTTP response code: 400")) {
            printError("Session cookie may be invalid")
          }
      } finally {
        if (in != null) {
          in.close()
        }
        connection.disconnect()
      }
    } else {
      println(s"Input file already exists for day $day")
    }
  }

  private def createDayScript(day: Int): Unit = {
    val dayPath = Paths.get(s"src/main/scala/day/Day${dayString(day)}.scala")
    if (!Files.exists(dayPath)) {
      val templatePath = Paths.get("src/main/scala/run/DayTemplate.scala")
      var fileContent = Files.readString(templatePath, StandardCharsets.UTF_8)
      fileContent = fileContent.replace("DayTemplate extends IDay(-1)", s"Day${dayString(day)} extends IDay($day)")
        .replace("DayTemplate", s"Day${dayString(day)}")
        .replace("package run", "package day\n\nimport run.{DayRunner, Result}\nimport utility.*")
      Files.writeString(dayPath, fileContent, StandardCharsets.UTF_8)
      println(s"Created script for day $day. ($dayPath)")
    } else {
      printError(s"Script file already exists for day $day. ($dayPath)")
    }
  }

  private def getInputPath(day: Int): Path = Paths.get(s"input/${dayString(day)}.txt")

  private def dayString(n: Int): String = f"$n%02d"

  def printError(message: String): Unit = {
    System.err.println(message)
  }
}
