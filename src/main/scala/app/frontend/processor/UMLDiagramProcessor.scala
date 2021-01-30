package app.frontend.processor

import app.frontend.InputPath
import app.frontend.exceptions.{BadInputPathException, BadOutputPathException, UMLConversionException}
import net.sourceforge.plantuml.{FileFormat, FileFormatOption, SourceStringReader}
import org.scalameta.UnreachableError
import org.slf4j.{Logger, LoggerFactory}
import pretty.config.PlantUMLConfig
import pretty.plantuml.UMLUnitPretty
import scalameta.toplevel.SourcesCollector
import uml.{UMLUnit, umlMethods}
import uml.umlMethods.{toAssocRep, toDistinctRep, toPackageRep}

import java.io.{File, FileNotFoundException, FileOutputStream, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.meta.parsers.Parsed
import scala.meta.{Source, dialects}
import scala.util.matching.Regex

sealed case class UMLDiagramProcessor(
                                       outputPath: String,
                                       filesPath: String,
                                       isVerbose: Boolean,
                                       isTextual : Boolean,
                                       name:String="default",
                                       exclude:Option[Regex]=None)
  extends Processor {

  override def execute(): UMLUnit = {

    val logger = LoggerFactory.getLogger("execution")

    val filesFound = getAllFiles(
      if(!filesPath.isEmpty) {
        val filePath = new File(filesPath)
        if(!filePath.exists() || !filePath.isDirectory()){
          throw new BadInputPathException("Input path does not exist. Try using --fp <path> with a valid path.")
        } else {
          filePath
        }
      } else {
        //If no explicit path for consumption of files is given
        //directory of executed jar is processed
        val path = ClassLoader.getSystemClassLoader().getResource(".").getPath
        logger.info(s"No input path specified. Assuming [$path] as input path.")
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


    val umlProcess = try {
      Some(SourcesCollector(parsedFiles,name))
    } catch {
      case ni:NotImplementedError =>
        throw new UMLConversionException(s"Files contain features that are not yet supported: ${ni.getMessage}",ni)
      case e:Exception =>
        throw new UMLConversionException(s"Unknown error when processing. try --verbose to get debug information.",e)
    }

     val res = for {
        umlCol <- umlProcess
      } yield {
        val path = if (outputPath.isEmpty) {
          logger.info(s"No output path specified. Assuming:" +
            s" ${ClassLoader.getSystemClassLoader.getResource(".").getPath} as output path." +
            s" Try --d <path> to define output path.")
          val path = ClassLoader.getSystemClassLoader().getResource(".").getPath
          path.replaceFirst("/","")
        } else {
          outputPath
        }

        implicit val prettyPrinter = UMLUnitPretty()(PlantUMLConfig())

        val rewritten = try {
          val dRep = toDistinctRep(umlCol.umlUnit).value
          val pRep = toPackageRep(dRep).value.asInstanceOf[UMLUnit]
          val cRep = umlMethods.insertCompanionObjects(pRep).value
          val aRep = toAssocRep(cRep).value.asInstanceOf[UMLUnit]
          val exRep = exclude.map(r => umlMethods.exclude(aRep,r).value).getOrElse(aRep).asInstanceOf[UMLUnit]
          exRep
        } catch {
          case e: Exception => throw e
        }

        if (!isTextual) {
          val reader = new SourceStringReader(rewritten.pretty)
          val filePath = new File(path)

          val fos = try {
            new FileOutputStream(new File(filePath.getPath + name + ".svg"))
          } catch {
            case fnf: FileNotFoundException => throw new BadOutputPathException(
              s"specified output path: [${filePath.getPath}] is invalid. Try --d <path> with a valid path.",
              fnf
            )
          }
          try {
            reader.generateImage(fos, new FileFormatOption(FileFormat.SVG))
            logger.info(s"Successfully exported image to location: ${filePath.getPath + name + ".svg"}")
            rewritten
          } catch {
            case i: IOException =>
              logger.error(s"Unable to export image: ${filePath.getPath + name + ".svg"}." +
                s" Try --verbose to get debug information.")
              logger.debug(s"${i.getStackTrace.mkString("Array(", ", ", ")")}")
              rewritten
          }
        } else {
          try {
            Files.write(Paths.get(path + (if(path.last.equals('/')){""}else{"/"}) + name + ".txt"), rewritten.pretty.getBytes(StandardCharsets.UTF_8))
            logger.info(s"Successfully exported text file to: ${path + "/" + name + ".txt"}")
            rewritten
          } catch {
            case i:IOException =>
              logger.error(s"Unable to export image: ${path + "/" + name + ".txt"}." +
                s" Try --verbose to get debug information.")
              logger.debug(s"${i.getStackTrace.mkString("Array(", ", ", ")")}")
              rewritten

          }
        }
      }
    res.get
  }

  private def parseTry(s: String):Option[Source] = {
    val logger: Logger = LoggerFactory.getLogger("execution")
    try {
      dialects.Scala3(new File(s)).parse[Source] match {
        case Parsed.Success(t) =>
          Some(t)
        case p: Parsed.Error =>
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
        logger.error(s"Parser could not successfully parse file [${s}]. try --verbose to get debug information.")
        logger.debug(s"Unreachable Error thrown due to an unreachable code sequence with message: ${u.getMessage} and stacktrace: ${u.printStackTrace}.")
        logger.warn(s"Definitions of file:[$s] will not be considered in final diagram.")
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
