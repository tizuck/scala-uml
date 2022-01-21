package app.ci

import app.ci.exceptions.ImplementationMissingException
import net.sourceforge.plantuml.{FileFormat, FileFormatOption, SourceStringReader}
import org.slf4j.LoggerFactory
import pretty.config.PlantUMLConfig
import pretty.plantuml.UMLUnitPretty
import scalameta.toplevel.SourcesCollector
import scopt.OParser
import uml.umlMethods.{toAssocRep, toPackageRep}
import uml.{UMLUnit, umlMethods}

import java.nio.charset.StandardCharsets.UTF_8
import java.io.{File,FileOutputStream, IOException}
import java.nio.file.{Files}

object Main extends App {
  import parserContainer._
  import umlProcessing._

  def execution(): Unit = {
    OParser.parse(parser, args, ParseConfig()) match {
      case Some(conf) =>
        println(conf)
        if(conf.in.nonEmpty){
          val procConf = Config(conf)
          val res = processUmlCol(procConf,tryUmlConstruction(procConf))
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
}
