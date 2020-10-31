import scalameta.PlantUMLCollector

import scala.meta._
import scala.meta.dialects

object Main extends App {
  val program =
    """
      | sealed trait UMLElement { self =>
      |  def pretty : String
      |}
      |
      |trait StereotypeElement extends UMLElement {
      |  val stereotype: String
      |  def foo():String = println("huhu")
      |}
      |
      |sealed trait TopLevelElement extends UMLElement
      |
      |sealed trait CompartmentElement extends UMLElement
      |
      |sealed trait PackageBodyElement extends UMLElement
      |
      |sealed trait RelateableElement extends UMLElement
      |
      |sealed trait NamedElement extends UMLElement {
      |  val identifier : String
      |}
      |
      |sealed case class GenericParameter(identifier:String,
      |                                   concreteType:Option[String],
      |                                   stereotype:Option[String]) extends
      |  StereotypeElement with
      |  NamedElement
      |
      |sealed case class UMLUnit(identifier:String,
      |                          toplevelElements:List[TopLevelElement]) extends UMLElement
      |
      |private[this] sealed case class Package(identifier:String,
      |                          packageBodyElements:List[PackageBodyElement],
      |                          stereotype:Option[String]) extends
      |  TopLevelElement with
      |  PackageBodyElement with
      |  StereotypeElement with
      |  NamedElement with
      |  RelateableElement
      |
      |  sealed case class Class(isAbstract:Boolean,
      |                        identifier:String,
      |                        attributes:List[Attribute],
      |                        operations:List[Operation],
      |                        additionalCompartements:List[Compartment],
      |                        genericParameters: Option[List[GenericParameter]],
      |                        stereotype : Option[String]) extends
      |  TopLevelElement with
      |  StereotypeElement with
      |  PackageBodyElement with
      |  RelateableElement with
      |  NamedElement
      |""".stripMargin
  val source = dialects.Dotty(program).parse[Source].get
  println(source.structure)
  val plantUMLUnit = PlantUMLCollector(source).plantUMLUnit
  println(plantUMLUnit)
  println(plantUMLUnit.pretty)
}


