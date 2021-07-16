package app.frontend2


import org.slf4j.{Logger, LoggerFactory}
import scopt.{OParser, OParserBuilder, OptionParser}

import java.io.File
import scala.meta.Source
import scala.meta.parsers.Parsed

object parserContainer {
  val builder: OParserBuilder[Config] = OParser.builder[Config]
  import builder._

  private val verbose =
    opt[Unit]('v',"verbose")
    .action((v,c) => c.copy(verbose = true))
    .text("Shows additional debug information.")

  private val files =
    opt[Seq[File]]('f',"files")
      .valueName("<path1>,<path2>,...")
      .validate { fs =>
        fs.foreach(f => println(f.isFile || f.isDirectory))
        fs.map{f =>
          if (f.isDirectory) {success}
          else if (f.isFile) {success}
          else {failure("-f <file1>,<file2>... must be files or directories.")}
        }.find(p => p.isLeft)
          .getOrElse(success)
      }
      .action{
        (d,c) =>
          c.copy(in = parseAllFiles(d))
      }
      .text("specify path where to find files. Can be single files or a directory full of files.")

  private def parseAllFiles(files:Seq[File]):List[(Source,String)] = {
    val logger = LoggerFactory.getLogger("execution")
    files
      .flatMap(f =>
        if(f.isFile && f.getName.endsWith(".scala")){
          logger.info(s"Final diagram includes file: ${f.getName}")
          List((parseFile(f),f.getPath + "." + f.getName)).filter(_._1.isEmpty)
        } else if(f.isDirectory){
          f
            .listFiles()
            .filter(_.isFile)
            .filter(_.getName.endsWith(".scala"))
            .map { df =>
              logger.info(s"Final diagram includes file: ${df.getName}")
              (parseFile(df),df.getPath + "." + df.getName)
            }.toList
        } else {
          Nil
        }
      ).toList
      .filter(t => t._1.nonEmpty)
      .map(t => (t._1.get,t._2))
  }

  private def parseFile(f:File):Option[Source] = {
    import scala.meta.dialects
    val logger : Logger = LoggerFactory.getLogger("execution")
    try {
      dialects.Scala3(f).parse[Source] match {
        case Parsed.Success(s) => Some(s)
        case error: Parsed.Error =>
          dialects.Scala213(f).parse[Source] match {
            case Parsed.Success(s) => Some(s)
            case error: Parsed.Error =>
              val msg =
                s"""File [${f.getName}] could not be interpreted as a Scala source.
                   | Continuing with other files.""".stripMargin
              logger.warn(msg)
              logger.debug(msg + s" Caused by: ${error.message} with stacktrace: ${error.details.toString}")
              None
          }
      }
    } catch {
      case e:Exception =>
        val msg = s"unrecognizable error while trying to parse file ${f.getName}"
        logger.warn(msg)
        logger.debug(msg + s"Caused by: ${e.getMessage} and stacktrace: ${e.toString}")
        None
    }
  }

  private val output =
    opt[File]('o',"output")
      .required()
      .valueName("<path>")
      .validate{f =>
        if(f.exists()) success
        else failure("<path> from option -o is invalid. It must be an existing directory.")
      }
      .action((o,c) => c.copy(out = o))
      .text("output path where generated UML diagram is stored.")

  private val textual =
    opt[Unit]('t',"textual")
    .action((t,c) => c.copy(textual = true))
    .text("No image but a text file containing the class diagram is generated.")

  private val name =
    opt[String]('n',"name")
      .action((n,c) => c.copy(name = n))
      .text("Name of the output file that is generated.")

  private val github =
    opt[File]('g',"github")
      .valueName("<path to github conf>")
      .validate(f =>
        if(f.isFile && f.getName.endsWith(".conf")){success}
        else failure(s"""Github config must be named "<name>.conf" and must be a file.""")
      )
      .text("specify to load a repository and produce uml diagram.")

  private val validityGithubXorFiles =
    checkConfig { c =>
      println(c)
      if (c.in.isEmpty && c.github.isEmpty) {
        failure("Either a file or a github repository must be provided as input using -f or -g respectively.")
      } else {
        success
      }
    }

  val parser: OParser[Unit, Config] = {
    import builder._
    OParser.sequence(
      programName("scala-uml"),
      verbose,
      files,
      output,
      textual,
      name,
      github,
      help('h',"help").text("prints all commands with explenation"),
    )
  }
}
