import scalameta.PlantUMLCollector

import scala.meta._
import scala.meta.dialects

object Main extends App {
  val program = """trait B {val a : Int; protected def foo():String ;val c :C}; trait C {val b:Bool}"""
  val source = dialects.Dotty(program).parse[Source].get
  val plantUMLUnit = PlantUMLCollector(source).plantUMLUnit
  println(plantUMLUnit)
  println(plantUMLUnit.pretty)
}


