package app.frontend.processor

import app.frontend.FilesPath
import net.sourceforge.plantuml.{FileFormat, FileFormatOption, SourceStringReader}
import pretty.config.PlantUMLConfig
import pretty.plantuml.UMLUnitPretty
import scalameta.toplevel.SourcesCollector

import java.io.{File, FileOutputStream, IOException}
import scala.meta.parsers.Parsed
import scala.meta.{Source, dialects}

sealed case class UMLDiagramProcessor(outputPath: String, filesPath: String, isVerbose: Boolean, name:String="default") extends Processor {
  override def execute(): Unit = {

    if(filesPath.isEmpty){
      throw new IOException("Please provide a directory for files to include" +
        " in the diagram via --directory command. Use --help for detailed info.")
    }

    val filesFound = getAllFiles(new File(filesPath))

    val parsedFiles = filesFound
      .map(s =>
        (dialects.Dotty(new File(s)).parse[Source] match {
          case Parsed.Success(t) => t
          case p:Parsed.Error =>
            throw new IOException("One or multiple files could not be interpreted" +
              s" as Scala files with error message: ${p.toString()}")}
          ,s))

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

  private def getAllFiles(file:File):List[String] = {
    file
      .listFiles()
      .filter(_.isFile)
      .filter(_.getName.endsWith(".scala"))
      .map(_.getPath)
      .toList
  }
}
