ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.9.0"

lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode2016"
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"
libraryDependencies += "com.lihaoyi" %% "upickle" % "4.4.3"
libraryDependencies += "com.google.ortools" % "ortools-java" % "9.15.6755"
libraryDependencies += "com.softwaremill.sttp.client4" %% "core" % "4.0.27"
