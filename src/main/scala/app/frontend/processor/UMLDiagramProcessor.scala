package app.frontend.processor

import app.frontend.FilesPath
import net.sourceforge.plantuml.{FileFormat, FileFormatOption, SourceStringReader}
import org.scalameta.UnreachableError
import org.slf4j.{Logger, LoggerFactory}
import pretty.config.PlantUMLConfig
import pretty.plantuml.UMLUnitPretty
import scalameta.toplevel.SourcesCollector

import java.io.{File, FileOutputStream, IOException}
import scala.meta.parsers.Parsed
import scala.meta.{Source, dialects}

sealed case class UMLDiagramProcessor(outputPath: String, filesPath: String, isVerbose: Boolean, name:String="default") extends Processor {
  override def execute(): Unit = {

    val logger = LoggerFactory.getLogger("execution")

    val filesFound = getAllFiles(
      if(!filesPath.isEmpty)
        new File(filesPath)
      else {
        //If no explicit path for consumption of files is given
        //directory of executed jar is processed
        val path = ClassLoader.getSystemClassLoader().getResource(".").getPath
        logger.info(s"No output path specified. Assuming [$path] as output path.")
        new File(path)
      }
    )

    for(file <- filesFound){
      logger.info(s"Found scala file: ${file} ")
    }

    val parsedFiles = for {
      (k, v)  <- filesFound.map(s => (parseTry(s), s))
      opt     <- k
    } yield {
      (opt,v)
    }


    val umlProcess = SourcesCollector(parsedFiles,name)

    implicit val prettyPrinter = UMLUnitPretty()(PlantUMLConfig())

    if(outputPath.isEmpty){

      val reader = new SourceStringReader(umlProcess.umlUnit.pretty)
      val filePath = new File("~/")

      val fos = new FileOutputStream(new File(filePath.getPath + name + ".svg"))
      reader.generateImage(fos,new FileFormatOption(FileFormat.SVG))
    } else {
      val reader = new SourceStringReader(umlProcess.umlUnit.pretty)
      val filePath = new File(outputPath)

      filePath.mkdirs()

      val fos = new FileOutputStream(new File(filePath.getPath + name + ".svg"))
      reader.generateImage(fos,new FileFormatOption(FileFormat.SVG))
    }
  }

  private def parseTry(s: String):Option[Source] = {
    val logger: Logger = LoggerFactory.getLogger("execution")
    try {
      dialects.Dotty(new File(s)).parse[Source] match {
        case Parsed.Success(t) =>
          Some(t)
        case p: Parsed.Error =>
          logger.warn(s"File: [$s] could not be processed as a Scala Source. Continuing with other files.")
          None
      }
    } catch {
      case u:UnreachableError =>
        logger.error(s"Parser could not successfully parse file [${s}]")
        logger.debug(s"Unreachable Error thrown due to an unreachable code sequence with message: ${u.getMessage} and cause: ${u.getCause}.")
        None
      case e:Exception =>
        logger.error(s"unrecognized error with message: ${e.getMessage}")
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
