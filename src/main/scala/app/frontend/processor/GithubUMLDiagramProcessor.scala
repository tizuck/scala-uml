package app.frontend.processor

import app.frontend.Filter
import app.frontend.exceptions.{GithubConfigFailedException, ImplementationMissingException}
import app.github.{GithubLoader, PublicGithub}

import org.slf4j.LoggerFactory

import pureconfig.ConfigReader.Result
import pureconfig.ConfigSource
import scalameta.toplevel.SourcesCollector
import uml.UMLUnit
import pureconfig._
import pureconfig.generic.auto._



case class GithubUMLDiagramProcessor(outputPath:String,
                                      githubConfigPath:String,
                                      isVerbose:Boolean,
                                      isTextual : Boolean,
                                      name:String="default",
                                      exclude:Option[Filter]=None)
  extends Processor {

  override def execute(): UMLUnit = {
    val logger = LoggerFactory.getLogger("execution")
    val githubRepoLoaded = ConfigSource.file(githubConfigPath).load[PublicGithub]
    val  githubRepo = getGithubConfigAtSuccess(githubRepoLoaded)
    val loadedGithub = getGithubRepoAtSuccess(githubRepo)
    logFoundScalaFiles(loadedGithub)
    val umlProcess = tryParseGithubFiles(loadedGithub)
    val res = processUmlCol(Some(umlProcess),logger, name, outputPath, isTextual, exclude)
    res.orNull
  }

  private def tryParseGithubFiles(loadedGithub: GithubLoader): SourcesCollector = {
    try {
      SourcesCollector(
        loadedGithub
          .repo
          .indexedFiles
          .map {
            case (s, sources) => (s, sources.headOption.getOrElse(throw new IllegalStateException()))
          }
          .toList
          .map(tp => tp.swap),
        name)
    } catch {
      case ni: NotImplementedError =>
        throw new ImplementationMissingException(
          "Construction of UML AST failed because of unimplemented features.", ni)
      case e: Exception =>
        throw new ImplementationMissingException(
          s"Unknown error when processing. try --verbose to get debug information.", e)
    }
  }

  private def getGithubRepoAtSuccess(githubRepo: PublicGithub): GithubLoader = {
    try {
      GithubLoader(githubRepo)
    } catch {
      case exception: Exception =>
        throw new GithubConfigFailedException(s"Config found at: [$githubConfigPath] is corrupt.", exception)
    }
  }

  private def getGithubConfigAtSuccess(githubRepoLoaded: Result[PublicGithub]): PublicGithub = {
    githubRepoLoaded match {
      case Left(_) =>
        throw new GithubConfigFailedException(s"Github config at: [$githubConfigPath] is corrupt.")
      case Right(dirs) => dirs
    }
  }

  private def logFoundScalaFiles(loadedGithub: GithubLoader): Unit = {
    val logger = LoggerFactory.getLogger("execution")
    loadedGithub.repo.indexedFiles.foreach {
      case (s, sources) =>
        val fileName = s
        sources.collectFirst(_ => true).getOrElse(throw new IllegalStateException(""))
        logger.info(s"Found scala file file: $fileName")
    }
  }
}
