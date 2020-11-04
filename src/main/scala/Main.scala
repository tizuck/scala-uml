import scalameta.UMLCollector
import scalameta.util.context.GlobalContext

import scala.meta._
import scala.meta.dialects

object Main extends App {
  val program =
    """
      |trait PartialFunction[-A, +B]
      |
      |""".stripMargin
  val source = dialects.Dotty(program).parse[Source].get
  println(source.structure)
  val plantUMLUnit = UMLCollector(source,GlobalContext(Map.empty)).plantUMLUnit
  println(plantUMLUnit)
  println(plantUMLUnit.pretty)
}


