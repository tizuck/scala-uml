/*
 * Copyright 2015 Tilman Zuckmantel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package app.github

import java.io.IOException
import java.util.concurrent.Executors
import cats.data.NonEmptyList
import cats.effect.{Blocker, ContextShift, IO}
import cats.implicits._
import cats.kernel.Semigroup
import github4s.Github
import github4s.domain.Content
import org.http4s.client.{Client, JavaNetClientBuilder}
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration.Duration
import scala.meta.{Source, dialects}
import scala.meta.parsers.Parsed
import scala.concurrent.duration

case class GithubLoader(repo:Repository)

object GithubLoader {

  private implicit val repositorySemiGroup:Semigroup[Repository] = {
    (x: Repository, y: Repository) =>
      Repository(x.indexedFiles |+| y.indexedFiles)
  }

  private val AMOUNT_SECONDS = 1
  private val TIMEOUT_DURATION = Duration(AMOUNT_SECONDS,duration.SECONDS)

  private val httpClient: Client[IO] = {
    val N_THREADS = 5
    val blockingPool = Executors.newFixedThreadPool(N_THREADS)
    val blocker = Blocker.liftExecutorService(blockingPool)
    implicit val cs: ContextShift[IO] = IO.contextShift(global)
    JavaNetClientBuilder[IO](blocker).create
  }

  private val accessToken = sys.env.get("GITHUB_TOKEN")
  private val gh = Github[IO](httpClient, accessToken)

  private def recieveFileEncoded(path:String)(implicit config:Config):Option[String] = {
    val recievedRawContent = getContents(path)
    if(!(recievedRawContent.size == 1) &&
      recievedRawContent.head.`type`.equals("file") &&
      recievedRawContent.head.encoding.nonEmpty) {
        throw new IllegalArgumentException(s"path: $path doesn't lead to a file")
    }
    recievedRawContent.head.content
  }

  private def decodeFileBase64( contentEncoded: Option[String]) = {
    contentEncoded
      .map(java.util.Base64.getMimeDecoder.decode(_).map(_.toChar).mkString)
      .getOrElse(throw new IllegalArgumentException(s"encoding $contentEncoded is corrupt"))
  }

  private def processRec(directory:Content)(implicit config:Config):Repository = {

    (directory.`type`,directory.content) match {

      case ("file",None) =>
        try {
          val parsedFile = processFile(directory)
          Repository(Map(directory.path -> List(parsedFile)))
        } catch {
          case e:Exception =>
            val logger = LoggerFactory.getLogger("execution")
            logger.warn(s"File: [$directory] could not be processed as a Scala Source. Continuing with other files.")
            logger.debug(s"File: [$directory] could not be processed as a Scala Source. Continuing with other files." +
              s" Caused by: ${e.getMessage} with stacktrace: ${e.getStackTrace.mkString("Array(", ", ", ")")}")
            Repository(Map.empty)
        }

      case ("file",Some(encodedFile)) =>
        val fileContent = decodeFileBase64(Some(encodedFile))
        val parsedFile = parseRecievedFile(fileContent)
        Repository(Map(directory.path -> List(parsedFile)))

      case ("dir",_) =>
        val getRawContents = getContents(directory.path)
        getRawContents.foldLeft(Repository.empty){
          case (acc,cont) => acc |+| processRec(cont)
        }

      case (s,_) => throw new NotImplementedError(s"unknown repository path type: $s")

    }
  }

  private def getContents(path:String)(implicit config:Config): NonEmptyList[Content] = config match {
    case p:PublicGithub =>
      val getContents = gh.repos.getContents(p.owner, p.name, path)

      val response = getContents
        .unsafeRunTimed(TIMEOUT_DURATION)
        .getOrElse(throw new IOException(s"Unable to fetch content of: $path"))

      val receivedRawContent = response.result match {
        case Left(e) => throw new IllegalStateException(e.toString)
        case Right(contents) => contents
      }
      receivedRawContent

    case _ => throw new NotImplementedError
  }
  @throws[IllegalArgumentException]("File is not parseable")
  private def processFile(content: Content)(implicit config: Config): Source = {
    val recievedFile = recieveFileEncoded(content.path)
    val parsedFile = parseRecievedFile(decodeFileBase64(recievedFile))

    parsedFile
  }

  @throws[IllegalArgumentException]("File is not parseable")
  private def parseRecievedFile(recievedFile: String): Source = {
    dialects
      .Scala3(recievedFile)
      .parse[Source] match {
      case Parsed.Success(t) => t
      case _:Parsed.Error =>
        dialects.Scala213(recievedFile).parse[Source] match {
          case Parsed.Success(t) => t
          case error: Parsed.Error =>
            throw new IllegalArgumentException(s"File $recievedFile could not be processed as Scala file.")
        }
    }
  }

  def apply(conf:Config): GithubLoader = conf match {
    case p:PublicGithub =>
      val repo = p
        .directories
        .dirEntries
        .foldLeft(Repository.empty){
          case (dirAcc,entry) =>
            val receivedRawContent: NonEmptyList[Content] = getContents(entry.path)(conf)
            val mergedParsedContents = receivedRawContent.foldLeft(Repository.empty){
              case (contAcc,content) =>
                contAcc |+| processRec(content)(conf)
            }
            dirAcc |+| mergedParsedContents
        }
      GithubLoader(repo)
    case _ => throw new NotImplementedError
  }


}

