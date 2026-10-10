import run.{DayRunner, IDay}

import java.io.File
import java.net.URL

object TestRunner {
  val dayRunners: Array[IDay] = {
    val classLoader = Thread.currentThread().getContextClassLoader
    val path = "day"
    val resources = classLoader.getResources(path.replace('.', '/')).asIterator()
    var resourcesList: List[URL] = Nil
    resources.forEachRemaining({x => resourcesList = x :: resourcesList})

    resourcesList.flatMap(resource => {
      val dir = new File(resource.toURI)

      if (!dir.exists || !dir.isDirectory) {
        Seq.empty
      } else {
        dir.listFiles
          .filter(_.getName.endsWith(".class"))
          .map(_.getName.replace(".class", ""))
          .flatMap { className =>
            val dayClass: Class[?] = Class.forName(s"$path.$className")
            if (dayClass.getInterfaces.contains(classOf[IDay])) {
              Some(className, dayClass.getDeclaredConstructor().newInstance().asInstanceOf[IDay])
            } else {
              None
            }
          }
      }
    }).sortBy(_._1).map(_._2).toArray
  }

  def main(args: Array[String]): Unit = {
    val n: Int = args(0).toInt
    runDayNumber(n)
  }

  def runDayNumber(n: Int): Unit = {
    runDay(dayRunners(n))
  }

  def runDay(day: IDay): Unit = {
    DayRunner(day).run()
  }
}
