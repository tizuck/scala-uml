import scalameta.UMLCollector
import scalameta.util.context.GlobalContext

import scala.meta._
import scala.meta.dialects

object Main extends App {
  val program =
    """
      |sealed case class Attribute(modificators:Option[List[Modificator]],
      |                            modifier: Option[AccessModifier],
      |                            identifier:String,
      |                            attributeType:Option[String],
      |                            stereotype:Option[String]) extends
      |  CompartmentElement with
      |  StereotypeElement with
      |  NamedElement
      |
      |sealed case class Parameter(identifier:String,
      |                            paramType:String,
      |                            stereotype:Option[String]) extends
      |  StereotypeElement with
      |  NamedElement
      |
      |
      |sealed case class Operation(modificator: Option[List[Modificator]],
      |                            accessModifier: Option[AccessModifier],
      |                            identifier:String,
      |                            paramSeq:List[List[Parameter]],
      |                            returnType:Option[String],
      |                            stereotype:Option[String]) extends
      |  CompartmentElement  with
      |  StereotypeElement with
      |  NamedElement
      |
      |sealed case class Compartment(identifier:Option[String],
      |                              compartmentElements:List[CompartmentElement],
      |                              stereotype:Option[String]) extends
      |  UMLElement with
      |  StereotypeElement
      |
      |/**
      | * Corresponds to a UML Note.
      | *
      | **/
      | sealed case class Note(attachedElements:List[NamedElement],
      |                        text:String,
      |                        stereotype:Option[String]) extends
      |  TopLevelElement with
      |  StereotypeElement with
      |  PackageBodyElement
      |
      |sealed trait RelationshipType
      |case object Extension extends RelationshipType
      |case object Composition extends RelationshipType
      |case object Aggregation extends RelationshipType
      |case object Annotation extends RelationshipType
      |case object Association extends RelationshipType
      |case object Inner extends RelationshipType
      |
      |sealed trait RelationshipDirection
      |case object FromTo extends RelationshipDirection
      |case object ToFrom extends RelationshipDirection
      |case object Without extends RelationshipDirection
      |
      |enum Color {
      |  case A,B,C,D,F,E
      |}
      |
      |
      |""".stripMargin
  val source = dialects.Dotty(program).parse[Source].get
  println(source.structure)
  val plantUMLUnit = UMLCollector(source,GlobalContext(Map.empty)).plantUMLUnit
  println(plantUMLUnit)
  println(plantUMLUnit.pretty)
}


