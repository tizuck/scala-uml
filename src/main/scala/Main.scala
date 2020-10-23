import scala.meta._
import scala.meta.dialects

object Main extends App {
  val program = """object Main extends App { enum Color {case Red, Green, Blue} }"""
  val tree = dialects.Dotty(program).parse[Source].get

  println(tree.structure)
}


