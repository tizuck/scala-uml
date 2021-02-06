package app.frontend

import app.frontend.exceptions.{BadInputPathException, BadOutputPathException, InvalidParameterException, NoParametersProvidedException}
import app.frontend.processor.{Processor, UMLDiagramProcessor}
import org.slf4j.LoggerFactory

import scala.util.Failure

object Main extends App {
  try {
    val log = LoggerFactory.getLogger("execution")
    val loader = Loader(args)
    val proc = Processor(loader.commands)
    proc.execute()
  } catch {
     case i:InvalidParameterException =>
       val log = LoggerFactory.getLogger("execution")
       log.error(i.getMessage)
       log.debug(s"${i.getMessage} caused by: ${i.getNoSucc()}")
     case n:NoParametersProvidedException =>
       val log = LoggerFactory.getLogger("execution")
       log.error(n.getMessage)
       log.debug(s"${n.getMessage} caused by: ${n.getStackTrace.mkString("Array(", ", ", ")")}")
     case e:Exception => logException(e)
  }


  def logException(e:Exception):Unit = {
    val log = LoggerFactory.getLogger("execution")
    log.error(e.getMessage)
    log.debug(s"${e.getMessage}" +
      s" and \n stacktrace: ${e.getStackTrace.mkString("Array(", ", ", ")")}" +
      (if(e.getCause != null){s"and cause: ${e.getCause.getStackTrace.mkString("Array(", ", ", ")")}"}else{""}))
  }
}
