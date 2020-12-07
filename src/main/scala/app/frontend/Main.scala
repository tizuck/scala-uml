package app.frontend

import app.frontend.processor.Processor

object Main extends App {
  val loader = Loader(args)
  Processor(loader.commands).execute()
}
