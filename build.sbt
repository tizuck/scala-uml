import Dependencies._

ThisBuild / scalaVersion     := "2.13.3"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

organizationName := "Tilman Zuckmantel"
startYear := Some(2015)
licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))

libraryDependencies ++= Seq(
    scalaTest % Test,
    "org.scalameta" %% "scalameta" % "4.3.24",
    "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.3.0",
    "org.typelevel" %% "cats-core" % "2.1.1",
    "net.sourceforge.plantuml" % "plantuml" % "8059",
    "com.github.marklister" %% "base64" % "0.2.10",
    "com.github.pureconfig" %% "pureconfig" % "0.14.0",
    "com.47deg" %% "github4s" % "0.27.1"
)

lazy val root = (project in file("."))
  .settings(
    name := "scala-uml",
  )