package app.frontend

import app.frontend.processor.Processor
import org.slf4j.LoggerFactory


object Main extends App {
  val log = LoggerFactory.getLogger("execution")
  try {
    val loader = Loader(args)
    val proc = Processor(loader.commands)
    proc.execute()
  } catch {
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
