import Dependencies._

ThisBuild / scalaVersion     := "2.13.3"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "scala-uml",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.scalameta" %% "scalameta" % "4.3.24",
    libraryDependencies += "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.3.0",
   // libraryDependencies += "org.scalameta" %% "common" % "4.3.24+30-63c1bd76-SNAPSHOT"

)