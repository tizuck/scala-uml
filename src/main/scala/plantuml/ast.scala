package plantuml

/**
 * @todo will be used later to define operations on all nodes
 */
sealed trait UMLElement { self =>
  def pretty : String = PrettyPrinter.format(self).layout
}

trait StereotypeElement extends UMLElement {
  val stereotype: Option[String]
}

sealed trait TopLevelElement extends UMLElement

sealed trait CompartmentElement extends UMLElement

sealed trait PackageBodyElement extends UMLElement

sealed case class UMLUnit(identifier:String, toplevelElements:List[TopLevelElement]) extends UMLElement

/***************
 * Packages
 **************/

sealed case class Package(identifier:String,
                          packageBodyElements:List[PackageBodyElement])(stereotypeN:Option[String]) extends {
  override val stereotype:Option[String] = stereotypeN
} with TopLevelElement with PackageBodyElement with StereotypeElement

/***************
 * Classes
 **************/

sealed case class GenericParameter(identifier:String,
                                   concreteType:String)(stereotypeN:Option[String]) extends {
  override val stereotype:Option[String] = stereotypeN
} with UMLElement with StereotypeElement

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
                        genericParameters: Option[List[GenericParameter]])
                       (stereotypeN:Option[String] = None) extends {
  override val stereotype:Option[String] = stereotypeN
}  with TopLevelElement with StereotypeElement with PackageBodyElement


/***************
 * Attributes
 **************/

sealed case class Attribute(modificators:Option[List[Modificator]],
                            modifier: Option[AccessModifier],
                            identifier:String,
                            attributeType:String)
                           (stereotypeN:Option[String]=None) extends {
  override val stereotype:Option[String] = stereotypeN
}  with CompartmentElement with StereotypeElement

/***************
 * Operations
 **************/

sealed case class Parameter(identifier:String,paramType:String)
                           (stereotypeN:Option[String] = None) extends {
  override val stereotype:Option[String] = stereotypeN
  } with StereotypeElement


sealed case class Operation(modificator: Option[List[Modificator]],
                            accessModifier: Option[AccessModifier],
                            identifier:String,
                            paramSeq:List[List[Parameter]],
                            returnType:Option[String])(stereotypeN:Option[String] = None) extends {
  override val stereotype = stereotypeN
  }  with CompartmentElement  with StereotypeElement


sealed case class Compartment(identifier:Option[String],
                              compartmentElements:List[CompartmentElement]) extends UMLElement

/***************
 * Notes
 **************/

sealed trait Position
case object Left extends Position
case object Right extends Position
case object Top extends Position
case object Bottom extends Position

/**
 * Corresponds to a UML Note.
 *
 **/
 sealed case class Note(text:String)(stereotypeN:Option[String] = None) extends {
  override val stereotype = stereotypeN
} with TopLevelElement with StereotypeElement with PackageBodyElement {

}

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
case object Note extends RelationshipType

sealed trait RelationshipDirection
case object FromTo extends RelationshipDirection
case object ToFrom extends RelationshipDirection
case object Without extends RelationshipDirection

sealed case class RelationshipInfo(sourceMultiplicity:Option[String],
                                   targetMultiplicity:Option[String],
                                   from: UMLElement,
                                   to: UMLElement,
                                   relationshipIdentifier:Option[String],
                                   identifierDirection:RelationshipDirection)

sealed case class Relationship(relationshipType: RelationshipType,
                               relationshipDirection: RelationshipDirection,
                               relationshipInfo: RelationshipInfo)(stereotypeN:Option[String] = None) extends {
  override val stereotype:Option[String] = stereotypeN
  } with TopLevelElement with PackageBodyElement with StereotypeElement
