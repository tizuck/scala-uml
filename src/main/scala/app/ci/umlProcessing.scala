package app.ci

import app.ci.Main.logException
import app.ci.exceptions.ImplementationMissingException
import net.sourceforge.plantuml.{FileFormat, FileFormatOption, SourceStringReader}
import org.slf4j.LoggerFactory
import pretty.config.PlantUMLConfig
import pretty.plantuml.UMLUnitPretty
import scalameta.toplevel.SourcesCollector
import uml.{UMLUnit, umlMethods}
import uml.umlMethods.{toAssocRep, toPackageRep}

import java.io.{File, FileOutputStream, IOException}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

object umlProcessing {
  @throws[ImplementationMissingException]("Construction of UML AST failed due to unimplemented features")
  def tryUmlConstruction(config: Config): Option[SourcesCollector] = {
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

  def processUmlCol(config:Config, sourcesCol:Option[SourcesCollector]):Option[UMLUnit] = {
    for {
      umlCol <- sourcesCol
    } yield {
      //@todo add the exclude option
      val rew = rewriteUMLAST(umlCol,config.filter)
      implicit val prettyPrinter: UMLUnitPretty = UMLUnitPretty()(PlantUMLConfig())
      val fileContent = rew.pretty
      val logger = LoggerFactory.getLogger("execution")
      if(config.textual){
        try {
          //Assumption: out is a path not a file (this is checked by the parser)
          val file = new File(config.out.getAbsolutePath + """\""" + config.name + ".puml")
          Files.write(file.toPath, fileContent.getBytes(UTF_8))
          logger.info(s"Successfully exported text file to: $file")
          rew
        } catch {
          case _:IOException =>
            logger.error(s"Could not write output file to path [${config.out}]." +
              s" Does the path exist and do you have the correct access rights ?")
            rew
          case e:Exception =>
            logException(e)
            rew
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

  def logException(e:Exception):Unit = {
    val log = LoggerFactory.getLogger("execution")
    log.error(e.getMessage)
    log.debug(s"${e.getMessage}" +
      s" and \n stacktrace: ${e.getStackTrace.mkString("Array(", ", ", ")")}" +
      (if(e.getCause != null){s"and cause: ${e.getCause.getMessage}, ${e.getCause.getStackTrace.mkString("Array(", ", ", ")")}"}else{""}))
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
