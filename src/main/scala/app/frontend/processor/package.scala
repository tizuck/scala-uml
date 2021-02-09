package app.frontend

import app.frontend.exceptions.BadOutputPathException
import net.sourceforge.plantuml.{FileFormat, FileFormatOption, SourceStringReader}
import org.slf4j.Logger
import pretty.config.PlantUMLConfig
import pretty.plantuml.UMLUnitPretty
import scalameta.toplevel.SourcesCollector
import uml.{UMLUnit, umlMethods}
import uml.umlMethods.{toAssocRep, toPackageRep}

import java.io.{File, FileNotFoundException, FileOutputStream, IOException}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, InvalidPathException, Paths}

package object processor {

  def determineOutputPath(logger: Logger, outputPath:String): String = {
    if (outputPath.isEmpty) {
      logger.info(s"No output path specified. Assuming:" +
        s" ${ClassLoader.getSystemClassLoader.getResource(".").getPath} as output path." +
        s" Try --d <path> to define output path.")
      val path = ClassLoader.getSystemClassLoader.getResource(".").getPath
      path.replaceFirst("/", "")
    } else outputPath
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

  def processTextualOutput(logger: Logger, path: String, rewritten: UMLUnit,name:String): UMLUnit = {
    implicit val prettyPrinter: UMLUnitPretty = UMLUnitPretty()(PlantUMLConfig())
    val filePath = path + (if (path.last.equals('/')) {""} else {"/"}) + name + ".puml"
    try {
      Files.write(Paths.get(filePath), rewritten.pretty.getBytes(UTF_8))
      logger.info(s"Successfully exported text file to: $filePath")
      rewritten
    } catch {
      case i: IOException =>
        logger.error(s"Unable to export image: $filePath." +
          s" Try --verbose to get debug information.")
        logger.debug(s"Unable to export image: $filePath." +
          s" Stacktrace: ${i.getStackTrace.mkString("Array(", ", ", ")")}")
        rewritten
      case i: InvalidPathException =>
        logger.error(s"Output path specified is not valid. Try using a valid output path with --d <path>")
        logger.debug(s"Output path specified is not valid. Try using a valid output path with --d <path>. " +
          s"Stacktrace: ${i.getStackTrace.mkString("Array(", ", ", ")")}")
        rewritten
    }
  }

  def processNonTextualOutput(logger: Logger, path: String, rewritten: UMLUnit,name:String): UMLUnit = {
    implicit val prettyPrinter: UMLUnitPretty = UMLUnitPretty()(PlantUMLConfig())
    val reader = new SourceStringReader(rewritten.pretty)
    val filePath = new File(path + (if (path.last.equals('/')) {""} else {"/"}))
    val file = new File(filePath.getPath + name + ".svg")
    val fos = try {
      new FileOutputStream(file)
    } catch {
      case _ : FileNotFoundException => throw new BadOutputPathException(
        s"specified output path: [${file.getPath}] is invalid. Try --d <path> with a valid path.")
    }
    try {
      reader.generateImage(fos, new FileFormatOption(FileFormat.SVG))
      logger.info(s"Successfully exported image to location: ${file.getPath}")
      rewritten
    } catch {
      case i: IOException =>
        logger.error(s"Unable to export image: ${filePath.getPath + name + ".svg"}." +
          s" Try --verbose to get debug information.")
        logger.debug(s"${i.getStackTrace.mkString("Array(", ", ", ")")}")
        rewritten
    }
  }
  def processUmlCol(umlProcess:Option[SourcesCollector],logger: Logger,name: String,outputPath:String,isTextual:Boolean,exclude: Option[Filter]): Option[UMLUnit] = {
    for {
      umlCol <- umlProcess
    } yield {
      val path = determineOutputPath(logger,outputPath)

      val rewritten = rewriteUMLAST(umlCol,exclude)

      if (!isTextual) {
        processNonTextualOutput(logger, path, rewritten,name)
      } else {
        processTextualOutput(logger, path, rewritten,name)
      }
    }
  }
}
