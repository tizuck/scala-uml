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

sealed trait ClassBodyElement extends UMLElement

sealed trait CompartmentElement extends UMLElement

sealed trait PackageBodyElement extends UMLElement

sealed case class UMLUnit(identifier:String, toplevelElements:List[TopLevelElement]) extends UMLElement

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
                          packageBodyElements:List[PackageBodyElement],
                          packageStyle: PackageStyle = Default) extends TopLevelElement with PackageBodyElement
/***************
 * Classes
 **************/

sealed case class GenericParameter(identifier:String,
                                   nested:Option[GenericParameter],
                                   lowerBound:Option[String],
                                   upperBound:Option[String]) extends UMLElement

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
                        classBodyElements:List[ClassBodyElement],
                        genericParameter: Option[GenericParameter],
                        symbolDepiction:Option[(String,String)])
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
} with ClassBodyElement with CompartmentElement with StereotypeElement

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
  } with ClassBodyElement with CompartmentElement  with StereotypeElement

sealed trait LineType
case object Single extends LineType
case object Dotted extends LineType
case object Double extends LineType
case object ThickSingle extends LineType

sealed case class Compartment(isHeading:Boolean,
                              lineType: LineType,
                              identifier:Option[String],
                              compartmentElements:List[CompartmentElement]) extends UMLElement

sealed case class CompartedClass(isAbstract:Boolean,
                                 identifier:String,
                                 genericParameter: Option[GenericParameter],
                                 symbolDepiction:Option[(String,String)],
                                 compartments:List[Compartment])(stereotypeN:Option[String]) extends {
  override val stereotype : Option[String] = stereotypeN
} with TopLevelElement with PackageBodyElement with StereotypeElement

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

sealed case class DirectionNote(position: Position, of:String,textN:String)(stereotypeN:Option[String]) extends {
  override val stereotype:Option[String] = stereotypeN
  override val text:String = textN
} with Note

sealed case class AliasNote(alias:String, textN:String)(stereotypeN:Option[String]) extends {
  override val stereotype:Option[String] = stereotypeN
  override val text:String = textN
} with Note

sealed case class LinkNote(position: Position, textN:String)(stereotypeN:Option[String]) extends {
  override val stereotype:Option[String] = stereotypeN
  override val text:String = textN
} with Note

/***************
 * Skinparams
 **************/

sealed case class SkinParam(args:List[String]) extends TopLevelElement

/***************
 * Hide
 **************/

sealed case class Hide(args:List[String]) extends TopLevelElement

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
                                   fromIdentifier:String,
                                   toIdentifier:String,
                                   relationshipIdentifier:Option[String],
                                   identifierDirection:RelationshipDirection)

sealed case class Relationship(relationshipType: RelationshipType,
                               relationshipDirection: RelationshipDirection,
                               relationshipInfo: RelationshipInfo)(stereotypeN:Option[String] = None) extends {
  override val stereotype:Option[String] = stereotypeN
  } with TopLevelElement with PackageBodyElement with StereotypeElement
