package app.frontend

import app.frontend.exceptions.{BadInputPathException, BadOutputPathException, InvalidParameterException, UMLConversionException}
import app.frontend.processor.{Processor, UMLDiagramProcessor}
import org.slf4j.LoggerFactory

import scala.util.Failure

object Main extends App {



  try {
    val loader = Loader(args)
    println(loader.commands)
    val proc = Processor(loader.commands)
    println(proc.toString)
    proc.execute()
  } catch {
     case i:InvalidParameterException =>
       val log = LoggerFactory.getLogger("execution")
       log.error(i.getMessage)
       log.debug(s"${i.getMessage} caused by: ${i.getNoSucc()}")
     case e:Exception => logException(e)
  }


  def logException(e:Exception):Unit = {
    val log = LoggerFactory.getLogger("execution")
    log.error(e.getMessage)
    log.debug(s"${e.getMessage} with cause: ${e.getCause}" +
      s" and stacktrace: ${e.getStackTrace.mkString("Array(", ", ", ")")}")
  }
}
