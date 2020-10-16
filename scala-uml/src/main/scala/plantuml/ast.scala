package plantuml

/**
 * @todo will be used later to define operations on all nodes
 */
sealed trait UMLElement

sealed trait TopLevelElement extends UMLElement

sealed trait RelateableElement extends UMLElement

sealed trait ClassBodyElement extends UMLElement

/***************
 * Classes
 **************/

sealed trait AccessModifier
case object Private extends AccessModifier
case object Protected extends AccessModifier
case object PackagePrivate extends AccessModifier
case object Public extends AccessModifier

sealed trait Modificator
case object Static extends Modificator
case object Abstract extends Modificator

sealed case class Parameter(identifier:String,paramType:String) extends UMLElement
sealed case class Operation(modificator: Modificator,
                            accessModifier: AccessModifier,
                            identifier:String,paramSeq:Seq[Seq[Parameter]],
                            returnType:String) extends ClassBodyElement


sealed case class Class(classBodyElements:Seq[ClassBodyElement]) extends RelateableElement with TopLevelElement
sealed case class CompartedClass() extends RelateableElement with TopLevelElement

/***************
 * Skinparams
 **************/

sealed case class SkinParam(args:Seq[String]) extends TopLevelElement

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
case object FromTo
case object ToFrom

class RelationshipInfo(fromMultiplicity:String,
                       sourceMultiplicity:String,
                       fromIdentifier:String,
                       toIdentifier:String,
                       relationshipIdentifier:String,
                       identifierDirection:RelationshipDirection)

sealed case class Relationship(relationshipType: RelationshipType,
                               relationshipDirection: RelationshipDirection,
                               relationshipInfo: RelationshipInfo,
                               from:RelateableElement,
                               to:RelateableElement) extends TopLevelElement
