package processors.repositories



import java.io.{File, FileOutputStream}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import java.util.concurrent._

import cats.data.NonEmptyList
import cats.effect.{Blocker, ContextShift, IO}
import cats.kernel.Semigroup
import cats.implicits._
import github4s.Github
import github4s.domain.Content
import net.sourceforge.plantuml.{FileFormat, FileFormatOption, SourceStringReader}
import org.http4s.{DecodeFailure, HttpVersion, Response}
import org.http4s.client.{Client, JavaNetClientBuilder}
import pretty.config.PlantUMLConfig
import pretty.plantuml.UMLUnitPretty

import scala.concurrent.ExecutionContext.global
import pureconfig._
import pureconfig.generic.auto._
import scalameta.toplevel.SourcesCollector

import scala.meta.{Dialect, Source}
import scala.meta.dialects
import scala.meta.parsers.Parsed



class KivyASTManipulatorTest extends AnyFreeSpec with Matchers {

  case class TestData() {
    val httpClient: Client[IO] = {
      val blockingPool = Executors.newFixedThreadPool(5)
      val blocker = Blocker.liftExecutorService(blockingPool)
      implicit val cs: ContextShift[IO] = IO.contextShift(global)
      JavaNetClientBuilder[IO](blocker).create // use BlazeClientBuilder for production use
    }

    val accessToken = sys.env.get("GITHUB_TOKEN")
    val gh = Github[IO](httpClient, accessToken)

    type FileName = String
    type FileContent = String

    case class Repository(indexedFiles:Map[FileName,List[Source]])

    object Repository {
      def empty():Repository = Repository(Map.empty)
    }

    implicit val repositorySemiGroup:Semigroup[Repository] = {
      (x: Repository, y: Repository) =>
        Repository(x.indexedFiles |+| y.indexedFiles)
    }

    val conf = ConfigSource.file("src/test/scala/processors.repositories/kivy-astmanip/kivy.conf").load[Directories]
    val directories = conf match {
      case Left(value) => throw new IllegalStateException(value.toString())
      case Right(dirs) => dirs
    }

    def recieveAndDecodeFile(path:String):FileContent = {
      val recievedRawContent = getContents(path)
      if(!(recievedRawContent.size == 1) &&
        recievedRawContent.head.`type`.equals("file") &&
        recievedRawContent.head.encoding.nonEmpty) {
        throw new IllegalArgumentException(s"path: $path doesn't lead to a file")
      }
      val fileEncoded = recievedRawContent.head.content
      decodeFileBase64(path, fileEncoded)
    }

    private def decodeFileBase64(path: String, contentEncoded: Option[String]) = {
      contentEncoded
        .map(java.util.Base64.getMimeDecoder.decode(_).map(_.toChar).mkString)
        .getOrElse(throw new IllegalArgumentException(s"encoding of file in path: $path is corrupt."))
    }

    val repo = directories
      .dirEntries
      .foldLeft(Repository.empty){
        case (dirAcc,entry) =>
          val receivedRawContent: NonEmptyList[Content] = getContents(entry.path)
          receivedRawContent.foldLeft(Repository.empty){
            case (contAcc,content) =>
              contAcc |+| processRec(content)
          }
      }

    private def processRec(directory:Content):Repository = {

      (directory.`type`,directory.encoding) match {
        case ("file",None) =>
          val (updatedPath: FileName, parsedFile: Source) = processFile(directory)
          Repository(Map(updatedPath -> List(parsedFile)))

        case ("file",Some(encodedFile)) =>
          val fileContent = decodeFileBase64(directory.path,Some(encodedFile))
          val parsedFile = parseRecievedFile(fileContent)
          Repository(Map(directory.path -> List(parsedFile)))

        case ("dir",_) =>
          val getRawContents = getContents(directory.path)
          getRawContents.foldLeft(Repository.empty()){
            case (acc,cont) => acc |+| processRec(cont)
          }
        case (s,_) => throw new NotImplementedError(s"unknown repository path type: ${s}")

      }

    }

    private def getContents(path:String) = {
      println(path)
      val getContents = gh.repos.getContents("theorydudes", "kivy-astmanip", path)
      val response = getContents.unsafeRunSync()
      val receivedRawContent = response.result match {
        case Left(e) => throw new IllegalStateException(e.toString)
        case Right(contents) => contents
      }
      receivedRawContent
    }

    private def processFile(content: Content) = {
      val recievedFile = recieveAndDecodeFile(content.path)
      val parsedFile = parseRecievedFile(recievedFile)
      (content.path, parsedFile)
    }

    private def parseRecievedFile(recievedFile: FileContent) = {
      dialects
        .Dotty(recievedFile)
        .parse[Source] match {
        case Parsed.Success(t) => t
        case Parsed.Error(position, str, exception) =>
          throw new IllegalArgumentException("File could not be interpreted as Scala File.")
      }
    }
  }

  implicit val umlUnit = UMLUnitPretty()(PlantUMLConfig())

  "KivyASTManip project from github is depicted correctly as uml diagram " in new TestData(){
    val sourcesCol = SourcesCollector(repo.indexedFiles.toList.map(tp => (tp._2.head,tp._1)),"kivy-astmanip-model")
    val reader = new SourceStringReader(sourcesCol.umlUnit.pretty.substring(0,sourcesCol.umlUnit.pretty.lastIndexOf("\n"))
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
      ))
    val filePath = new File("src/test/scala/assets/out/kivy-astmanip")

    filePath.mkdirs()

    val fos = new FileOutputStream(new File(filePath.getPath +  "/structure.svg"))
    val sec = reader.generateImage(fos,new FileFormatOption(FileFormat.SVG))
    sec must not be null
  }

}
