import java.io.{File, FileOutputStream}
import java.nio.charset.StandardCharsets
import java.util.concurrent.Executors

import cats.effect.{Blocker, ContextShift, IO}
import github4s.Github
import net.sourceforge.plantuml.{FileFormat, FileFormatOption, SourceStringReader}
import org.http4s.client.{Client, JavaNetClientBuilder}
import scalameta.SourceCollector
import scalameta.toplevel.SourcesCollector
import scalameta.util.context.GlobalContext
import scalameta.util.namespaces.scalaDefaults
import scalameta.util.util.statToString

import scala.concurrent.ExecutionContext.global
import scala.meta.Defn.{Class, Object, Trait}
import scala.meta._
import scala.meta.dialects

object Main extends App {

  val httpClient: Client[IO] = {
    val blockingPool = Executors.newFixedThreadPool(5)
    val blocker = Blocker.liftExecutorService(blockingPool)
    implicit val cs: ContextShift[IO] = IO.contextShift(global)
    JavaNetClientBuilder[IO](blocker).create // use BlazeClientBuilder for production use
  }

  val encodingHard = "N0LnJld3JpdGUoewogICAqICAgIGNhc2UgUHl0aG9uKCJbMSwy\nLDNdIikgPT4gUHl0aG9uKCJbMSwyLDMsNF0iKQogICAqICAgfSkKICAgKn19\nfQogICAqCiAgICogUGxlYXNlIG5vdGUsIHRoYXQgQVNUTm9kZXMgY2FuIG5v\ndCBiZSByZXdyaXR0ZW4gYXJiaXRyYXJpbHkuIFNpbmNlIGVhY2ggQVNUTm9k\nZSBpbXBsaWVzCiAgICogYSBzcGVjaWZpYyBwYXJhbWV0ZXIgbGlzdC4gQW4g\nQVNUIGhhcyB0byBzdGF5IHN0cnVjdHVyZS1jb25zaXN0ZW50IGFmdGVyIGFw\ncGx5aW5nIHJld3JpdGluZyBydWxlcy4KICAgKiBBIHJld3JpdGluZyBydWxl\nIGFzOgogICAqIHt7ewogICAqICAgewogICAqICAgIGNhc2UgUHl0aG9uKHMp\nID0+IFRvcExldmVsKE5pbCkKICAgKiAgIH0KICAgKiB9fX0KICAgKiBpcyBu\nb3QgdmFsaWQgYXMgYSBbW21vZGVsLlRvcExldmVsXV0tbm9kZSBjYW4gbm90\nIG9jY3VyIGF0IHBvc2l0aW9ucyB3aGVyZSBhIFtbbW9kZWwuUHl0aG9uXV0t\nbm9kZSBjYW4uCiAgICoKICAgKiBAc2VlIFtbaHR0cHM6Ly9iaXRidWNrZXQu\nb3JnL2lua3l0b25pay9raWFtYS9zcmMvMDMyNjMwZmEyMWRkYWQ1Y2YzM2Ni\nZDZlZjljMmYwMjc4NjYxYTY3NS93aWtpL1Jld3JpdGluZy5tZF1dCiAgICog\nQHBhcmFtIGZwIFBhcnRpYWwgZnVuY3Rpb24gdGhhdCBkZWZpbmVzIGhvdyB0\naGUgYXN0IHNob3VsZCBiZSByZXdyaXR0ZW4uCiAgICogQHJldHVybiBBIHJl\nd3JpdHRlbiBBU1QgYWNjb3JkaW5nIHRvIHRoZSBzcGVjaWZpY2F0aW9uIGlu\nIGBmcGAgb3IgdGhlIHNhbWUgYXN0IGlmIGBmcGAgY291bGQgbm90IGJlIGFw\ncGxpZWQuCiAgICovCiAgZGVmIHJld3JpdGUoZnA6QVNUTm9kZSA9PT4gQVNU\nTm9kZSk6IEFTVE5vZGUgPSBzZWxmLnRyYXZlcnNlQW5kQXBwbHkocnVsZShm\ncCkpCgogIC8qKgogICAqIFRyYW5zZm9ybXMgYHNlbGZgIGludG8gYSB3ZWxs\nIGZvcm1hdHRlZCBraXZ5IHByb2dyYW0gdGhhdCBjYW4gYmUgd3JpdHRlbgog\nICAqIGludG8gYSBmaWxlLgogICAqCiAgICogVGhlIGZvbGxvd2luZyBBU1RO\nb2RlIGZvciBleGFtcGxlOgogICAqIHt7ewogICAqICAgVG9wTGV2ZWwoCiAg\nICogICAgTGlzdCgKICAgKiAgICAgIFJvb3QoCiAgICogICAgICAgIFdpZGdl\ndCgKICAgKiAgICAgICAgICBQbG90LAogICAqICAgICAgICAgIExpc3QoCiAg\nICogICAgICAgICAgICBXaWRnZXQoCiAgICogICAgICAgICAgICAgIExpbmVH\ncmFwaCwKICAgKiAgICAgICAgICAgICAgTGlzdCgKICAgKiAgICAgICAgICAg\nICAgICBQcm9wZXJ0eShiYWNrZ3JvdW5kX25vcm1hbCxMaXN0KCcnKSksCiAg\nICogICAgICAgICAgICAgICAgUHJvcGVydHkoYmFja2dyb3VuZF9jb2xvcixM\naXN0KFswLDAsMCwxXSkpCiAgICogICApKSkpKSkpCiAgICogfX19CiAgICoK\nICAgKiBpcyBwcmludGVkOgogICAqIHt7ewogICAqIFBsb3Q6CiAgICogIExp\nbmVHcmFwaDoKICAgKiAgICBiYWNrZ3JvdW5kX25vcm1hbDogJycKICAgKiAg\nICBiYWNrZ3JvdW5kX2NvbG9yOiBbMCwwLDAsMV0KICAgKiB9fX0KICAgKgog\nICAqIEByZXR1cm4gQSBmb3JtYXR0ZWQgQVNUTm9kZSB0aGF0IGNhbiBiZSBp\nbnRlcnByZXRlZCBhcyBhIEtpdnkgZmlsZS4KICAgKi8KICBkZWYgcHJldHR5\nOlN0cmluZyA9IEtpdnlQcmV0dHlQcmludGVyLmZvcm1hdChzZWxmKS5sYXlv\ndXQKfQ=="

  val accessToken = sys.env.get("GITHUB_TOKEN")
  val gh = Github[IO](httpClient, accessToken)

  val path = "src/main/scala/com/github/theorydudes/model/ASTNode.scala"

  val getContents = gh.repos.getContents("theorydudes","kivy-astmanip", path)
  val response = getContents.unsafeRunSync()
  val content = response.result match {
    case Left(e) => throw e
    case Right(contents) => contents.head
  }
  println(content)
  println("-----------------------------------")
  println(content.content.get)
  val contentEncoded = content.content.get
  println(java.util.Base64.getDecoder.decode(encodingHard))
  println("-----------------------------------")
  println("-----TRY TO DECODE WITH JAVA-------")
  println("-----------WITH STRING-------------")
  println("-----------------------------------")
  try {
    println(java.util.Base64.getDecoder.decode(contentEncoded))
  } catch {
    case e:Exception => println(e.getMessage)
  }
  println("-----------------------------------")
  println("-----TRY TO DECODE WITH JAVA-------")
  println("-----------WITH BYTEARRAY----------")
  println("-----------------------------------")
  try {
    println(java.util.Base64.getDecoder.decode(contentEncoded.getBytes))
  } catch {
    case e:Exception => println(e.getMessage)
  }
  println("-----------------------------------")
  println("-----TRY TO DECODE WITH JAVA-------")
  println("-----------WITH BYTEARRAY UTF8----------")
  println("-----------------------------------")
  try {
    println(java.util.Base64.getDecoder.decode(contentEncoded.getBytes(StandardCharsets.UTF_8)))
  } catch {
    case e:Exception => println(e.getMessage)
  }
  println("-----------------------------------")
  println("-----TRY TO DECODE WITH JAVA-------")
  println("-----------WITH URL DECODER----------")
  println("-----------------------------------")
  try {
    println(java.util.Base64.getUrlDecoder.decode(contentEncoded))
  } catch {
    case e:Exception => println(e.getMessage)
  }
  println("-----------------------------------")
  println("-----TRY TO DECODE WITH JAVA-------")
  println("-----------WITH URL DECODER BYTES----------")
  println("-----------------------------------")
  try {
    println(java.util.Base64.getUrlDecoder.decode(contentEncoded.getBytes(StandardCharsets.UTF_8)))
  } catch {
    case e:Exception => println(e.printStackTrace())
  }
  println("-----------------------------------")
  println("-----TRY TO DECODE WITH JAVA-------")
  println("-----------WITH MiME DECODER----------")
  println("-----------------------------------")
  try {
    val newlineChar =
    println(contentEncoded.replaceAll("\\n", "").replaceAll("\\r", ""))
    println(java.util.Base64.getMimeDecoder.decode(contentEncoded.replaceAll("\\n", "").replaceAll("\\r", "")))
  } catch {
    case e:Exception => println(e.printStackTrace())
  }

  /*val foo =
    """
      |package foo
      |
      |trait A
      |""".stripMargin

  val bar = """
    |package bar
    |
    |import foo._
    |
    |trait B extends A
    |
    |""".stripMargin

  val bar2 = """
              |package bar2
              |
              |import foo._
              |import bar._
              |
              |trait C extends A
              |
              |""".stripMargin
  /*val path = java.nio.file.Paths.get("src","main", "scala","uml", "ast.scala")
  val bytes = java.nio.file.Files.readAllBytes(path)
  val text = new String(bytes, "UTF-8")
  val input = Input.VirtualFile(path.toString, text)

  val astSource = input.parse[Source].get*/
  val fooSource = dialects.Dotty(foo).parse[Source].get
  val barSource = dialects.Dotty(bar).parse[Source].get
  val bar2Source = dialects.Dotty(bar2).parse[Source].get


  val umlCollector = SourcesCollector(List((fooSource,"foo.scala"),(barSource,"bar.scala"),(bar2Source,"bar2.scala")),"foobar")

  val plantUMLUnit = umlCollector.umlUnit



  //println(plantUMLUnit)
  //println(plantUMLUnit.structure)

  //hack some skinparams into the files

  val prettyFile = plantUMLUnit
    .pretty
    .substring(0,plantUMLUnit.pretty.lastIndexOf("\n"))
    .appended('\n')
    .appendedAll(
      """
        |skinparam linetype ortho
        |
        |skinparam class {
        | Backgroundcolor white
        | Bordercolor black
        | Arrowcolor Black
        |}
        |
        |hide circle
        |@enduml
        |""".stripMargin
    )
  val reader = new SourceStringReader(prettyFile)
  val filePath = new File("diaOut")

  filePath.mkdirs()

  val fos = new FileOutputStream(new File(filePath.getPath + "/ast.svg"))
  val sec = reader.generateImage(fos,new FileFormatOption(FileFormat.SVG))
  println(plantUMLUnit.pretty)
 // println("-----------------------------------------")
 // println(plantUMLUnit.pretty)
 */
}


