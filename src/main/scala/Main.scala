import scalameta.PlantUMLCollector

import scala.meta._
import scala.meta.dialects

object Main extends App {
  val program = """trait B {val a : Int}"""
  val source = dialects.Dotty(program).parse[Source].get
  val plantUMLUnit = PlantUMLCollector(source).plantUMLUnit
  println(plantUMLUnit)
  println(plantUMLUnit.pretty)
}


