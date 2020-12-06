package app.frontend

object Main extends App {
  val loader = Loader(args)
  try {
    println(Checker.checkCommandList(loader.commands))
  } catch {
    case e:Exception => println("[error]" + e.getMessage)
  }
}
