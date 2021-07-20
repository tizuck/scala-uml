package app.ci


import org.slf4j.{Logger, LoggerFactory}
import scopt.{OParser, OParserBuilder, OptionParser}

import java.io.File
import scala.meta.Source
import scala.meta.parsers.Parsed

object parserContainer {
  val builder: OParserBuilder[ParseConfig] = OParser.builder[ParseConfig]
  import builder._

  private val verbose =
    opt[Unit]('v',"verbose")
    .action((v,c) => c.copy(verbose = true))
    .text("Shows additional debug information.")

  private val exclude =
    opt[String]('e',"exclude")
      .action((v,c) => c.copy(filter = Some(v)))
      .text("Exclude option to exclude UML elements from the diagram." +
        " Elements are matched on their package and name.\n" +
        "It is possible to define filters and concatenate them together.\n" +
        "Opterators to concatenate are: <filter> ::= And(<filter>;<filter>),Or(<filter>;<filter>)\n" +
        ",Not(<filter>),R(<regex>). Where <regex> is a regular expression.\n")

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
          c.copy(in = d)
      }
      .text("specify path where to find files. Can be single files or a directory full of files.")



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

  val parser: OParser[Unit, ParseConfig] = {
    import builder._
    OParser.sequence(
      programName("scala-uml"),
      verbose,
      files,
      output,
      textual,
      name,
      github,
      exclude,
      validityGithubXorFiles,
      help('h',"help").text("prints all commands with explanation"),
    )
  }
}
