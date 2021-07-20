package app.ci

import org.bitbucket.inkytonik.kiama.parsing.{ListParsers, NoSuccess, Success}
import org.bitbucket.inkytonik.kiama.util.{Positions, StringSource}
import org.slf4j.{Logger, LoggerFactory}

import java.io.File
import scala.meta.Source
import scala.meta.parsers.Parsed
import scala.util.matching.Regex

/**
 * Config class that summarizes all **processed** information of input flags.
 *
 * Use the apply method of this class to transform an instance of the [[ParseConfig]]
 * class to an instance of this class.
 *
 * @param verbose Additional debug information should be shown.
 * @param out Validated output directory.
 * @param in Validated and parsed sequence of scala input files together with their names
 *           for further processing.
 * @param textual Output should not be an image but a text file.
 * @param name Validated name of the output file.
 * @param github Accessible Github repository containing scala classes.
 */
case class Config(verbose:Boolean,
                  out:File,
                  in : Seq[(Source,String)],
                  textual : Boolean,
                  name : String,
                  github : Option[File],
                  filter:Option[Filter])

object Config {
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

  class Rules(positions:Positions) extends ListParsers(positions) {

    //override def whitespace: Parser[Any] = regex("[\\s]*".r)

    lazy val filter : Parser[Filter] = and | or | not | exclude

    lazy val and : Parser[And] = "And" ~> "(" ~> filter ~ (";" ~> filter <~ ")") ^^ And
    lazy val or : Parser[Or] = "Or" ~> "(" ~> filter ~ (";" ~> filter <~ ")") ^^ Or
    lazy val not : Parser[Not] = "Not" ~> "(" ~> filter <~ ")" ^^ Not
    lazy val exclude : Parser[Exclude] = reg ^^ Exclude

    lazy val reg : Parser[Regex] = "R" ~> "{" ~> ".+?(?=})".r <~ "}" ^^ { r => r.r}
  }

  private def parseFilter(s:Option[String]):Option[Filter] = {
    for{
      filter <- s
    } yield {
      val rules = new Rules(new Positions)
      rules.parseAll(rules.filter, StringSource(filter)) match {
        case Success(result, _) => println(result);Some(result)
        case fail: NoSuccess =>
          val logger = LoggerFactory.getLogger("execution")
          logger.warn(s"Pattern for Exclude can not be parsed with reason: [$fail]")
          None
      }
    }
  }.flatten

  /**
   * Transforms an instance of the [[ParseConfig]] class into an instance of this class.
   *
   * During the transformation the input files stored in [[ParseConfig.in]] are validated and
   * parsed.
   *
   * @param conf Unprocessed and partially not validated configuration.
   * @return Fully validated configuration.
   */
  def apply(conf:ParseConfig):Config = {
    val inFiles = parseAllFiles(conf.in)
    val pFilter = parseFilter(conf.filter)
    new Config(
      verbose = conf.verbose,
      out = conf.out,
      in = inFiles,
      textual = conf.textual,
      name = conf.name,
      github = conf.github,
      filter = pFilter
    )
  }
}
