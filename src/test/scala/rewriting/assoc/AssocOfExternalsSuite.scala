package rewriting.assoc


import org.scalactic.PrettyMethods.Prettyizer
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import pretty.config.PlantUMLConfig
import pretty.plantuml.UMLUnitPretty
import scalameta.toplevel.SourcesCollector
import uml.{UMLUnit, umlMethods}

import scala.meta.{Source, dialects}

class AssocOfExternalsSuite() extends AnyFreeSpec with Matchers {
  val program =
    """
      |package foo {
      |  trait FooExpr extends (Unit => Unit) {
      |    val b : Boolean
      |    val c : Int
      |    val a : FooExpr
      |    val d : Unit => Unit
      |  }
      |}
      |""".stripMargin

  val parsedProgram = dialects.Scala3(program).parse[Source].get
  val collectedUml = SourcesCollector(List((parsedProgram,"foo.scala")),"foo")

  "Associations that reference to external entities are diplayed as attributes" in {
    val rewritten = umlMethods.toAssocRep(collectedUml.umlUnit).value.asInstanceOf[UMLUnit]
    implicit val pretty = UMLUnitPretty()(PlantUMLConfig())
    println(rewritten.pretty)
  }
}
