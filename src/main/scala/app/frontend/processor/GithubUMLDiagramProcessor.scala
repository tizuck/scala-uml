package app.frontend.processor

import app.frontend.Filter
import app.frontend.exceptions.{BadInputPathException, BadOutputPathException, GithubConfigFailedException, UMLConversionException}
import app.github.{GithubLoader, PublicGithub}
import net.sourceforge.plantuml.{FileFormat, FileFormatOption, SourceStringReader}
import org.slf4j.LoggerFactory
import pretty.config.PlantUMLConfig
import pretty.plantuml.UMLUnitPretty
import pureconfig.ConfigSource
import scalameta.toplevel.SourcesCollector
import uml.{ClassRef, Inner, Relationship, RelationshipInfo, UMLUnit, umlMethods}
import uml.umlMethods.{toAssocRep, toDistinctRep, toPackageRep}
import pureconfig._
import pureconfig.generic.auto._
import uml.externalReferences.ClassDefRef

import java.io.{File, FileNotFoundException, FileOutputStream, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.util.matching.Regex

case class GithubUMLDiagramProcessor(
                                      outputPath:String,
                                      githubConfigPath:String,
                                      isVerbose:Boolean,
                                      isTextual : Boolean,
                                      name:String="default",
                                      exclude:Option[Filter]=None)
  extends Processor {

  override def execute(): UMLUnit = {
    val logger = LoggerFactory.getLogger("execution")

    val githubRepoLoaded = ConfigSource.file(githubConfigPath).load[PublicGithub]

    val  githubRepo = githubRepoLoaded match {
      case Left(value) =>
        logger.debug(s"Github config at: [$githubConfigPath] with message: ${value.toString()}")
        throw new BadInputPathException(s"Github config at: [$githubConfigPath] is corrupt.")
      case Right(dirs) => dirs
    }
    val loadedGithub = try {
      GithubLoader(githubRepo)
    } catch {
      case exception: Exception =>
        throw new GithubConfigFailedException(s"Config found at: [$githubConfigPath] is corrupt.",exception)
    }

    logFoundScalaFiles(loadedGithub)

    val umlProcess = try {
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
    }  catch {
      case ni:NotImplementedError =>
        throw new UMLConversionException(s"Files contain features that are not yet supported: ${ni.getMessage}",ni)
      case e:Exception =>
        throw new UMLConversionException(s"Unknown error when processing. try --verbose to get debug information.")
    }

    val path = if(outputPath.isEmpty){
      logger.info(s"No output path specified. Assuming:" +
        s" ${ClassLoader.getSystemClassLoader.getResource(".").getPath} as output path." +
        s" Try --d <path> to define output path.")
        ClassLoader.getSystemClassLoader.getResource(".").getPath
      } else {
        outputPath
      }

    implicit val prettyPrinter: UMLUnitPretty = UMLUnitPretty()(PlantUMLConfig())
    val rewritten = try {
      val dRep = toDistinctRep(umlProcess.umlUnit).value
      val pRep = toPackageRep(dRep).value.asInstanceOf[UMLUnit]
      val cRep = umlMethods.insertCompanionObjects(pRep).value
      val aRep = toAssocRep(cRep).value.asInstanceOf[UMLUnit]
      val exRep = exclude.map(r => umlMethods.exclude(aRep,r).value).getOrElse(aRep).asInstanceOf[UMLUnit]
      exRep
    } catch {
      case e: Exception => throw e
    }

    val reader = new SourceStringReader(rewritten.pretty)
    val filePath = new File(path)

    if(!isTextual) {
      val fos = try {
        if (filePath.isDirectory) {
          new FileOutputStream(new File(filePath.getPath + "/" + name + ".svg"))
        } else {
          new FileOutputStream(new File(filePath.getPath + name + ".svg"))
        }
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
    }  else {
      try {
        Files.write(Paths.get(path + name + ".puml"), rewritten.pretty.getBytes(StandardCharsets.UTF_8))
        logger.info(s"Successfully exported PlantUML file to location: $path$name.puml")
        rewritten
      } catch {
        case i:IOException =>
          logger.error(s"Unable to export image: ${path + name + ".txt"}." +
            s" Try --verbose to get debug information.")
          logger.debug(s"${i.getStackTrace.mkString("Array(", ", ", ")")}")
          rewritten
      }
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
