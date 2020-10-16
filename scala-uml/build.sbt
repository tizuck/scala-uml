import Dependencies._

ThisBuild / scalaVersion     := "2.13.3"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "scala-uml",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.scalameta" %% "scalameta" % "4.3.24"
  )