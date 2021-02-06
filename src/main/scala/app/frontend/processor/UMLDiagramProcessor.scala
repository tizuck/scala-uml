package app.frontend.processor

import app.frontend.Filter
import app.frontend.exceptions.{BadInputPathException, BadOutputPathException, ImplementationMissingException, NoParametersProvidedException}
import net.sourceforge.plantuml.{FileFormat, FileFormatOption, SourceStringReader}
import org.scalameta.UnreachableError
import org.slf4j.{Logger, LoggerFactory}
import pretty.config.PlantUMLConfig
import pretty.plantuml.UMLUnitPretty
import scalameta.toplevel.SourcesCollector
import uml.{UMLUnit, umlMethods}
import uml.umlMethods.{toAssocRep, toDistinctRep, toPackageRep}

import java.io.{File, FileNotFoundException, FileOutputStream, IOException}
import java.nio.charset.StandardCharsets._
import java.nio.file.{Files, InvalidPathException, Paths}
import scala.meta.parsers.Parsed
import scala.meta.{Source, dialects}

sealed case class UMLDiagramProcessor(
                                       outputPath: String = "",
                                       filesPath: String = "",
                                       isVerbose: Boolean = false,
                                       isTextual : Boolean = false,
                                       name:String="default",
                                       exclude:Option[Filter]=None)
  extends Processor {

  override def execute(): UMLUnit = {

    val logger = LoggerFactory.getLogger("execution")

    val filesFound = getAllFiles(determineInputFilePath(logger))

    logFoundFiles(logger, filesFound)

    val parsedFiles = tryToParseFiles(filesFound)

    if(parsedFiles.isEmpty){
      throw new BadInputPathException(s"Files in directory $filesPath are not interpretable as Scala programs.")
    }

    val umlProcess = tryUmlConstruction(logger, parsedFiles)

    val res = processUmlCol(umlProcess,logger,name,outputPath, isTextual, exclude)
    res.getOrElse(null)
  }




  private def rewriteUMLAST(umlCol: SourcesCollector): UMLUnit = {
    try {
      val dRep = toDistinctRep(umlCol.umlUnit).value
      val pRep = toPackageRep(dRep).value.asInstanceOf[UMLUnit]
      val cRep = umlMethods.insertCompanionObjects(pRep).value
      val aRep = toAssocRep(cRep).value.asInstanceOf[UMLUnit]
      val exRep = exclude.map(r => umlMethods.exclude(aRep, r).value).getOrElse(aRep).asInstanceOf[UMLUnit]
      exRep
    } catch {
      case e: Exception => throw e
    }
  }

  private def determineOutputPath(logger: Logger): String = {
    if (outputPath.isEmpty) {
      logger.info(s"No output path specified. Assuming:" +
        s" ${ClassLoader.getSystemClassLoader.getResource(".").getPath} as output path." +
        s" Try --d <path> to define output path.")
      val path = ClassLoader.getSystemClassLoader.getResource(".").getPath
      path.replaceFirst("/", "")
    } else outputPath
  }

  private def tryUmlConstruction(logger: Logger, parsedFiles: List[(Source, String)]): Option[SourcesCollector] = {
    try {
      Some(SourcesCollector(parsedFiles, name))
    } catch {
      case ni: NotImplementedError =>
        throw new ImplementationMissingException(
          "Construction of UML AST failed because of unimplemented features.",ni)
      case e: Exception =>
        throw new ImplementationMissingException(
          s"Unknown error when processing. try --verbose to get debug information.", e)
    }
  }

  private def tryToParseFiles(filesFound: List[String]): List[(Source, String)] = {
    for {
      (k, v) <- filesFound.map(s => (parseTry(s), s))
      opt <- k
    } yield {
      (opt, v)
    }
  }

  private def logFoundFiles(logger: Logger, filesFound: List[String]): Unit = {
    for (file <- filesFound) {
      logger.info(s"Found scala file: $file ")
    }
  }

  @throws[BadInputPathException]("Input path is not a directory or does not exist")
  private def determineInputFilePath(logger: Logger): File = {
    if (filesPath.nonEmpty) {
      val filePath = new File(filesPath)
      if (!filePath.exists() || !filePath.isDirectory) {
        throw new BadInputPathException("Input path does not exist or is not a directory." +
          " Try using --fp <path> with a valid path.")
      } else {
        filePath
      }
    } else {
      //If no explicit path for consumption of files is given
      //directory of executed jar is processed
      val path = ClassLoader.getSystemClassLoader.getResource(".").getPath
      logger.info(s"No input path specified. Assuming [$path] as input path.")
      new File(path)
    }
  }

  private def parseTry(s: String):Option[Source] = {
    val logger: Logger = LoggerFactory.getLogger("execution")
    try {
      dialects.Scala3(new File(s)).parse[Source] match {
        case Parsed.Success(t) =>
          Some(t)
        case _: Parsed.Error =>
          dialects.Scala213(new File(s)).parse[Source] match {
            case Parsed.Success(t) =>
              Some(t)
            case p: Parsed.Error =>
              logger.warn(s"File: [$s] could not be processed as a Scala Source. Continuing with other files.")
              logger.debug(s"File: [$s] could not be processed as a Scala Source. Continuing with other files." +
                s" Caused by: ${p.message} with stacktrace: ${p.details.getStackTrace.mkString("Array(", ", ", ")")}")
              None
          }
      }
    } catch {
      case u:UnreachableError =>
        logger.warn(s"Definitions of file:[$s] will" +
          s" not be considered in final diagram because file is not interpretable.")
        logger.debug(s"Unreachable Error thrown due to an unreachable code sequence with message:" +
          s" ${u.getMessage} and stacktrace: ${u.printStackTrace()}.")
        None
      case e:Exception =>
        logger.warn(s"unrecognized error while trying to process file: $s with message: ${e.getMessage}")
        logger.debug(s"unrecognized error while trying to process file: $s with message:" +
          s" ${e.getMessage} and stacktrace: {${e.getStackTrace.mkString("Array(", ", ", ")")}}")
        None
    }
  }

  private def getAllFiles(file:File):List[String] = {
    file
      .listFiles()
      .filter(_.isFile)
      .filter(_.getName.endsWith(".scala"))
      .map(_.getPath)
      .toList
  }
}
