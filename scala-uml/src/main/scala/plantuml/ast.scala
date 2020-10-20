package plantuml

/**
 * @todo will be used later to define operations on all nodes
 */
sealed trait UMLElement

trait StereotypeElement extends UMLElement {
  val stereotype: Option[String] = None
}

sealed trait TopLevelElement extends UMLElement

sealed trait ClassBodyElement extends UMLElement

sealed trait CompartmentElement extends UMLElement

sealed trait PackageBodyElement extends UMLElement

sealed case class PlantUMLUnit(identifier:String,toplevelElements:Seq[TopLevelElement])

/***************
 * Packages
 **************/

sealed trait PackageStyle
case object Default extends PackageStyle
case object Node extends PackageStyle
case object Rectangle extends PackageStyle
case object Folder extends PackageStyle
case object Frame extends PackageStyle
case object Cloud extends PackageStyle
case object Database extends PackageStyle

sealed case class Package(identifier:String,
                          color:Option[String],
                          packageBodyElements:Seq[PackageBodyElement])
                         (packageStyle: PackageStyle = Default) extends TopLevelElement with PackageBodyElement
/***************
 * Classes
 **************/

sealed case class GenericParameter(identifier:String,
                                   nested:Option[GenericParameter],
                                   lowerBound:String,
                                   upperBound:String) extends UMLElement

sealed trait AccessModifier
case object Private extends AccessModifier
case object Protected extends AccessModifier
case object PackagePrivate extends AccessModifier
case object Public extends AccessModifier

sealed trait Modificator
case object Static extends Modificator
case object Abstract extends Modificator

sealed case class Class(isAbstract:Boolean,
                        classBodyElements:Seq[ClassBodyElement],
                        genericParameter: GenericParameter,
                        symbolDepiction:Option[(String,String)])
                       (stereotype:Option[String] = None) extends {
  override val stereotype = stereotype
}  with TopLevelElement with StereotypeElement with PackageBodyElement

/***************
 * Attributes
 **************/

sealed case class Attribute(identifier:String,attributeType:String)(stereotype:Option[String]=None) extends {
  override val stereotype = stereotype
} with ClassBodyElement with CompartmentElement with StereotypeElement

/***************
 * Operations
 **************/

sealed case class Parameter(identifier:String,paramType:String)
                           (stereotype:Option[String] = None) extends {
  override val stereotype = stereotype
  } with StereotypeElement


sealed case class Operation(modificator: Modificator,
                            accessModifier: AccessModifier,
                            identifier:String,paramSeq:Seq[Seq[Parameter]],
                            returnType:String)(stereotype:Option[String] = None) extends {
  override val stereotype = stereotype
  } with ClassBodyElement with CompartmentElement  with StereotypeElement

sealed trait LineType
case object Single extends LineType
case object Dotted extends LineType
case object Double extends LineType
case object ThickSingle extends LineType

sealed case class Compartment(isHeading:Boolean,
                              lineType: LineType,
                              identifier:Option[String],
                              compartmentElements:Seq[CompartmentElement]) extends UMLElement

sealed case class CompartedClass(compartments:Seq[Compartment]) extends  TopLevelElement with PackageBodyElement

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
 * notes can be produced using the following syntax in PlantUML:
 * ```
 * note ::= 'note' ('top of' | 'left of' | 'right of' | 'bottom of') classIdentifier ':' text
 * note ::= text 'as' identifier
 * ```
 *
 * Examples:
 * ```
 * note top of Object : In java, every class\nextends this one.
 * -> Note(None,Some((Top,"Object")),"In java, every class\nextends this one")
 *
 * note "This is a floating note" as N1
 * -> Note(Some("N1"),None,"This is a floating note")
 * ```
 *
 * For Relationships the `Relationship` class is used in a special manner:
 * ```
 * Object .. N2
 * -> Relationship(Note,"","","Object","N1","")
 * ```
 *
 */
 trait Note extends TopLevelElement with StereotypeElement with PackageBodyElement {
  val text : String
}

sealed case class DirectionNote(position: Position, of:String,text:String)(stereotype:Option[String]) extends {
  override val stereotype = stereotype
  override val text = text
} with Note

sealed case class AliasNote(alias:String, text:String)(stereotype:Option[String]) extends {
  override val stereotype = stereotype
  override val text = text
} with Note

sealed case class LinkNote(position: Position, of:String,text:String)(stereotype:Option[String]) extends {
  override val stereotype = stereotype
  override val text = text
} with Note

/***************
 * Skinparams
 **************/

sealed case class SkinParam(args:Seq[String]) extends TopLevelElement

/***************
 * Hide
 **************/

sealed case class Hide(args:Seq[String]) extends TopLevelElement

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
case object FromTo
case object ToFrom
case object Without

class RelationshipInfo(fromMultiplicity:String,
                       sourceMultiplicity:String,
                       fromIdentifier:String,
                       toIdentifier:String,
                       relationshipIdentifier:String,
                       identifierDirection:RelationshipDirection)

sealed case class Relationship(relationshipType: RelationshipType,
                               relationshipDirection: RelationshipDirection,
                               relationshipInfo: RelationshipInfo)(stereotype:Option[String] = None) extends {
  override val stereotype = stereotype
  } with TopLevelElement with PackageBodyElement with StereotypeElement
