package app.frontend2


import app.frontend.exceptions.ImplementationMissingException
import app.frontend.processor.processUmlCol
import org.slf4j.{Logger, LoggerFactory}
import scalameta.toplevel.SourcesCollector
import scopt.OParser

object Main extends App {
  import parserContainer._

  def execution(): Unit = {
    OParser.parse(parser, args, Config()) match {
      case Some(conf) =>
        if(conf.in.nonEmpty){
          val col = tryUmlConstruction(conf)
          //TODO: Give this a new wrapping, just for now it is sufficient
          val res = processUmlCol(col,LoggerFactory.getLogger("execution"),
            conf.name,conf.out.getAbsolutePath,conf.textual,None)
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

  def logException(e:Exception):Unit = {
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

}
