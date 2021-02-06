package rewriting.companion

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import pretty.config.PlantUMLConfig
import pretty.plantuml.UMLUnitPretty
import scalameta.toplevel.SourcesCollector
import uml.{Annotation, Association, ClassRef, ConcreteClass, FromTo, Relationship, Without, umlMethods}

import scala.meta.{Source, dialects}

class CompanionObjectSuite extends AnyFreeSpec with Matchers {
  val program: String =
    """
      |package foo {
      |  trait FooExpr
      |  sealed case class Foo(i:Int)
      |  object Foo {
      |    val fooExpr : FooExpr = new FooExpr { }
      |    def apply(i:Int):Foo = new Foo(i)
      |  }
      |}
      |""".stripMargin

  val parsedProgram: Source = dialects.Scala3(program).parse[Source].get
  val collectedUml: SourcesCollector = SourcesCollector(List((parsedProgram,"foo.scala")),"foo")

  "companion object is correctly depicted in resulting UML class diagram" in {
    val umlUnit = collectedUml.umlUnit
    val rewritten = umlMethods.insertCompanionObjects(umlUnit)

    implicit val pretty: UMLUnitPretty = UMLUnitPretty()(PlantUMLConfig())

    //Test if companion object is correctly created
    rewritten.value.exists{
      case c:uml.Class => c.name.equals("$Foo") && c.stereotype.exists(s => s.name.equals("object"))
      case _ => false
    } must be(true)
    //Test if case class still exists
    rewritten.value.exists{
      case c:uml.Class => c.name.equals("Foo") && c.stereotype.exists(s => s.name.equals("caseclass"))
      case _ => false
    } must be(true)
    //Test if companion dependency relationship exists
    rewritten.value.exists{
      case r:Relationship => r.relationshipType.equals(Annotation) &&
        r.stereotype.exists(s => s.name.equals("companion")) &&
        r.relationshipInfo.from.exists{
          case ConcreteClass(c) => c.name.equals("Foo")
          case _ => false
        }
        r.relationshipInfo.to.exists{
          case ClassRef("$Foo",_) => true
          case _ => false
        }
        r.relationshipDirection.equals(Without)
      case _ => false
    } must be(true)
    //Test if association to FooExpr is outgoing from companion object

    println(rewritten.value.pretty)
    rewritten.value.exists{
      case r:Relationship =>
        r.relationshipType.equals(Association) &&
        r.relationshipDirection.equals(FromTo) &&
        r.relationshipInfo.from.exists{case c:ClassRef => c.name.equals("$Foo") case _ => false} &&
        r.relationshipInfo.to.exists{case c:ClassRef => c.name.equals("FooExpr") case _ => false} &&
        r.relationshipInfo.relationshipIdentifier.exists(s => s.equals("fooExpr ")) &&
        r.relationshipInfo.targetMultiplicity.exists(s => s.equals("1"))
      case _ => false
    } must be(true)

  }

}
