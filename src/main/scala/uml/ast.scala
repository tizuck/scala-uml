package uml

import plantuml.SimplePlantUMLPrettyPrinter

/**
 * @todo will be used later to define operations on all nodes
 */
sealed trait UMLElement { self =>
  def pretty : String = SimplePlantUMLPrettyPrinter.format(self).layout
}

trait StereotypeElement extends UMLElement {
  val stereotype: Option[String]
}

sealed trait TopLevelElement extends UMLElement

sealed trait CompartmentElement extends UMLElement

sealed trait PackageBodyElement extends UMLElement

sealed trait RelateableElement extends UMLElement

sealed trait NamedElement extends UMLElement {
  val identifier : String
}

sealed case class UMLUnit(identifier:String,
                          toplevelElements:List[TopLevelElement]) extends UMLElement

/***************
 * Packages
 **************/

sealed case class Package(identifier:String,
                          packageBodyElements:List[PackageBodyElement],
                          stereotype:Option[String]) extends
  TopLevelElement with
  PackageBodyElement with
  StereotypeElement with
  NamedElement with
  RelateableElement

/***************
 * Classes
 **************/

sealed case class GenericParameter(identifier:String,
                                   concreteType:Option[String],
                                   stereotype:Option[String]) extends
  StereotypeElement with
  NamedElement

sealed trait AccessModifier
case object Private extends AccessModifier
case object Protected extends AccessModifier
case object PackagePrivate extends AccessModifier
case object Public extends AccessModifier

sealed trait Modificator
case object Static extends Modificator
case object Abstract extends Modificator

sealed case class Class(isAbstract:Boolean,
                        identifier:String,
                        attributes:List[Attribute],
                        operations:List[Operation],
                        additionalCompartements:List[Compartment],
                        genericParameters: Option[List[GenericParameter]],
                        stereotype : Option[String]) extends
  TopLevelElement with
  StereotypeElement with
  PackageBodyElement with
  RelateableElement with
  NamedElement
/***************
 * Attributes
 **************/

sealed case class Attribute(modificators:Option[List[Modificator]],
                            modifier: Option[AccessModifier],
                            identifier:String,
                            attributeType:Option[String],
                            stereotype:Option[String]) extends
  CompartmentElement with
  StereotypeElement with
  NamedElement

/***************
 * Operations
 **************/

sealed case class Parameter(identifier:String,
                            paramType:String,
                            stereotype:Option[String]) extends
  StereotypeElement with
  NamedElement


sealed case class Operation(modificator: Option[List[Modificator]],
                            accessModifier: Option[AccessModifier],
                            identifier:String,
                            paramSeq:List[List[Parameter]],
                            returnType:Option[String],
                            stereotype:Option[String]) extends
  CompartmentElement  with
  StereotypeElement with
  NamedElement


sealed case class Compartment(identifier:Option[String],
                              compartmentElements:List[CompartmentElement],
                              stereotype:Option[String]) extends
  UMLElement with
  StereotypeElement

/**
 * Corresponds to a UML Note.
 *
 **/
 sealed case class Note(attachedElements:List[NamedElement],
                        text:String,
                        stereotype:Option[String]) extends
  TopLevelElement with
  StereotypeElement with
  PackageBodyElement

/***************
 * Relationships
 **************/

sealed trait RelationshipType
case object Extension extends RelationshipType
case object Composition extends RelationshipType
case object Aggregation extends RelationshipType
case object Annotation extends RelationshipType
case object Association extends RelationshipType
case object Inner extends RelationshipType

sealed trait RelationshipDirection
case object FromTo extends RelationshipDirection
case object ToFrom extends RelationshipDirection
case object Without extends RelationshipDirection

sealed case class RelationshipInfo(sourceMultiplicity:Option[String],
                                   targetMultiplicity:Option[String],
                                   from: RelateableElement with NamedElement,
                                   to: RelateableElement with NamedElement,
                                   relationshipIdentifier:Option[String],
                                   identifierDirection:RelationshipDirection)

sealed case class Relationship(relationshipType: RelationshipType,
                               relationshipDirection: RelationshipDirection,
                               relationshipInfo: RelationshipInfo,
                               stereotype:Option[String]) extends
  TopLevelElement with
  PackageBodyElement with
  StereotypeElement
