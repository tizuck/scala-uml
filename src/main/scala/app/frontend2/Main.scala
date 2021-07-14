package app.frontend2


import app.frontend.Filter
import app.frontend.exceptions.ImplementationMissingException
import net.sourceforge.plantuml.{FileFormat, FileFormatOption, SourceStringReader}
import org.slf4j.LoggerFactory
import pretty.config.PlantUMLConfig
import pretty.plantuml.UMLUnitPretty
import scalameta.toplevel.SourcesCollector
import scopt.OParser
import uml.umlMethods.{toAssocRep, toPackageRep}
import uml.{UMLUnit, umlMethods}

import java.nio.charset.StandardCharsets.UTF_8
import java.io.{File, FileNotFoundException, FileOutputStream}
import java.nio.file.{Files, Path, Paths}

object Main extends App {
  import parserContainer._

  def execution(): Unit = {
    OParser.parse(parser, args, Config()) match {
      case Some(conf) =>
        println(conf)
        if(conf.in.nonEmpty){
          val col = tryUmlConstruction(conf)
          //TODO: Give this a new wrapping, just for now it is sufficient
          val res = processUmlCol(conf,col)
          res.orNull
        }
      case None => println("naa")
    }
  }

  try {
    execution()
  } catch {
    case e:Exception => logException(e)
  }

  private def logException(e:Exception):Unit = {
    val log = LoggerFactory.getLogger("execution")
    log.error(e.getMessage)
    log.debug(s"${e.getMessage}" +
      s" and \n stacktrace: ${e.getStackTrace.mkString("Array(", ", ", ")")}" +
      (if(e.getCause != null){s"and cause: ${e.getCause.getMessage}, ${e.getCause.getStackTrace.mkString("Array(", ", ", ")")}"}else{""}))
  }

  @throws[ImplementationMissingException]("Construction of UML AST failed due to unimplemented features")
  private def tryUmlConstruction(config: Config): Option[SourcesCollector] = {
    try {
        Some(SourcesCollector(config.in.toList, config.name))
    } catch {
      case ni: NotImplementedError =>
        throw new ImplementationMissingException(
          "Construction of UML AST failed because of unimplemented features.",ni)
      case e: Exception =>
        throw new ImplementationMissingException(
          s"Unknown error when processing. try --verbose to get debug information.", e)
    }
  }

  private def processUmlCol(config:Config,sourcesCol:Option[SourcesCollector]):Option[UMLUnit] = {
    for {
      umlCol <- sourcesCol
    } yield {
      //@todo add the exclude option
      val rew = rewriteUMLAST(umlCol,None)
      implicit val prettyPrinter: UMLUnitPretty = UMLUnitPretty()(PlantUMLConfig())
      val fileContent = rew.pretty
      val logger = LoggerFactory.getLogger("execution")
      if(config.textual){
        try {
          //Assumption: out is a path not a file (this is checked by the parser)
          val file = new File(config.out.getAbsolutePath + config.name + ".puml")
          Files.write(file.toPath, fileContent.getBytes(UTF_8))
          logger.info(s"Successfully exported text file to: $file")
          rew
        } catch {
          case e:Exception =>
            logException(e)
            throw new IllegalArgumentException
        }
      } else {
        val reader = new SourceStringReader(rew.pretty)
        val fos = try {
          new FileOutputStream(new File(config.out.getAbsolutePath + config.name + ".svg"))
        } catch {
          case e:Exception =>
            logException(e)
            //@todo provide better exception
            throw new IllegalArgumentException()
        }
        try {
          reader.generateImage(fos, new FileFormatOption(FileFormat.SVG))
          logger.info(s"Successfully exported image to location: ${config.out.getPath}")
          rew
        } catch {
          case e:Exception =>
            logException(e)
            rew
        }
      }
    }
  }

  def rewriteUMLAST(umlCol: SourcesCollector, exclude: Option[Filter]): UMLUnit = {
    try {
      val pRep = toPackageRep(umlCol.umlUnit).value.asInstanceOf[UMLUnit]
      val cRep = umlMethods.insertCompanionObjects(pRep).value
      val aRep = toAssocRep(cRep).value.asInstanceOf[UMLUnit]
      val exRep = exclude.map(r => umlMethods.exclude(aRep, r).value).getOrElse(aRep).asInstanceOf[UMLUnit]
      exRep
    } catch {
      case e: Exception => throw e
    }
  }
}
