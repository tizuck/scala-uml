import scalameta.PlantUMLCollector

import scala.meta._
import scala.meta.dialects

object Main extends App {
  val program =
    """
      |trait A(a:Int)(implicit c : CollectorContext) {
      | val a : Int
      | val b : Bool
      | var c : List
      | type E
      |}
      |
      |trait Int
      |
      |trait B {
      |  def foo(numberValid:Map[Int,Bool])(implicit guaranteedValidNumber : Int) : Int
      |}
      |
      |trait C extends A with B
      |""".stripMargin
  val source = dialects.Dotty(program).parse[Source].get
  println(source.structure)
  val plantUMLUnit = PlantUMLCollector(source).plantUMLUnit
  println(plantUMLUnit)
  println(plantUMLUnit.pretty)
}


