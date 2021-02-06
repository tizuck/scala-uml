package rewriting.assoc


import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import pretty.config.PlantUMLConfig
import pretty.plantuml.UMLUnitPretty
import scalameta.toplevel.SourcesCollector
import uml._

import scala.meta.{Source, dialects}

class AssocOfExternalsSuite() extends AnyFreeSpec with Matchers {
  val program: String =
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

  val parsedProgram: Source = dialects.Scala3(program).parse[Source].get
  val collectedUml: SourcesCollector = SourcesCollector(List((parsedProgram,"foo.scala")),"foo")

  "Associations that reference to external entities are diplayed as attributes" in {
    val rewritten = umlMethods.toAssocRep(collectedUml.umlUnit).value.asInstanceOf[UMLUnit]
    implicit val pretty: UMLUnitPretty = UMLUnitPretty()(PlantUMLConfig())

    rewritten.exists{
      case c:uml.Class =>
        c.name.equals("FooExpr") &&
        c.attributes.exists(a => a.name.equals("b") && a.attributeType.contains("Boolean"))
      case _ => false
    } must be(true)

    rewritten.exists{
      case c:uml.Class =>
        c.name.equals("FooExpr") &&
          c.attributes.exists(a => a.name.equals("c") && a.attributeType.contains("Int"))
      case _ => false
    } must be(true)

    rewritten.exists{
      case c:uml.Class =>
        c.name.equals("FooExpr") &&
          c.attributes.exists(a => a.name.equals("d") && a.attributeType.contains("Function1<Unit,Unit>"))
      case _ => false
    } must be(true)

    rewritten.exists{
      case r:Relationship =>
        r.relationshipType.equals(Association) &&
        r.relationshipInfo.from.asInstanceOf[ClassRef].name.equals("FooExpr") &&
          r.relationshipInfo.to.asInstanceOf[ClassRef].name.equals("FooExpr") &&
        r.relationshipInfo.relationshipIdentifier.isDefined &&
        r.relationshipInfo.relationshipIdentifier.get.equals("a ")
      case _ => false
    } must be(true)
  }
}
