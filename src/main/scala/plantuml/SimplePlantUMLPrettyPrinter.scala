package plantuml

import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
import java.util.UUID.randomUUID
import uml.{Abstract, AccessModifier, Aggregation, Annotation, Association, Attribute, Class, Compartment,
  Composition, Extension, FromTo, GenericParameter, Inner, Modificator, Note, Operation, Package, PackagePrivate,
  Parameter, Private, Protected, Public, Relationship, RelationshipDirection, RelationshipInfo, RelationshipType,
  Static, StereotypeElement, ToFrom, UMLElement, UMLUnit, Without}

/**
 * @todo Remove empty string from relationship
 * @todo cleanup opt mess
 */
object SimplePlantUMLPrettyPrinter extends org.bitbucket.inkytonik.kiama.output.PrettyPrinter {

  def format[T<:UMLElement](t: T) : Document = {

    super.pretty(show(t))
  }

  def show(umlElement: UMLElement): Doc = umlElement match {

    case UMLUnit(
    identifier,
    toplevelElements) =>
      "@startuml" <+>
        stringWrap(identifier) <@>
        vsep(toplevelElements.map(show)) <@>
        "@enduml"

    case Package(
    identifier,
    packageBodyElements,stereotype) =>
      "package" <+>
        stringWrap(stereotype.getOrElse("") + identifier) <+>
        enclose("{",nest(line <> vsep(packageBodyElements.map(show))),line <> "}")

    case GenericParameter(
    identifier,
    concreteType,stereotype) =>
        opt(stereotype,text) <>
        identifier <>
          opt(concreteType,text,space <> ":" <> space,emptyDoc)

    case Class(
    isAbstract,
    identifier: String,
    attributes,
    operations,
    additionalCompartements,
    genericParameter,
    stereotype) =>
      (if(isAbstract) {"abstract" <> space } else { emptyDoc }) <>
      "class" <+>
        stringWrap(identifier) <>
        opt(genericParameter,  (gps:List[GenericParameter]) => hsep(gps.map(show),sep = ','),emptyR = space) <>
        opt(stereotype,text,"<<" <> space,space <> ">>") <>
        (if(attributes.nonEmpty || operations.nonEmpty || additionalCompartements.nonEmpty) {
          enclose("{",
            nest(line <>
              vsep(attributes.map(show)) <>
              (if (attributes.nonEmpty) line else emptyDoc) <>
              vsep(operations.map(show)) <>
              (if (operations.nonEmpty) line else emptyDoc) <>
              vsep(additionalCompartements.map(show))
            ),
             "}")
        } else emptyDoc)

    case a@Attribute(
    modificators,
    accessModifier,
    identifier,
    attributeType, _) =>
      opt(accessModifier,showAccessModifier,r=emptyDoc) <>
        showStereotype(a) <>
        opt(modificators,showModificators) <>
        identifier <+> ':' <+> attributeType

    case p@Parameter(
    identifier,
    paramType,_) =>
      showStereotype(p) <>
        identifier <+>
        ':' <+>
        paramType

    case o@Operation(
    modificators,
    accessModifier,
    identifier,
    paramSeq,
    returnType,_) =>
      showStereotype(o) <>
        opt(modificators,showModificators) <>
        opt(accessModifier,showAccessModifier) <>
        identifier <>
        hsep(paramSeq.map(params => '(' <> hsep(params.map(show),", ") <> ')')) <+>
        opt(returnType,text,":" <> space,r = emptyDoc)

    case Compartment(identifier, compartmentElements,stereotype) =>
        "--" <>
          opt(stereotype,text,space,emptyDoc)
          opt(identifier,text,l=space,r=space <> "--") <>
          line <>
          vsep(compartmentElements.map(show))

    case Note(attachedElements,nText,stereotype) =>
      val noteId = randomUUID().toString

      "note" <+>
      surround('"', nText) <+>
      "as" <+>
      noteId <>
      (if (attachedElements.nonEmpty) {
        line <>
          vsep(attachedElements.map(show))
      } else {
        emptyDoc
      })


    case r@Relationship(
    relationshipType,
    relationshipDirection,
    RelationshipInfo(sourceMultiplicity,
      targetMultiplicity,
      from,
      to,
      relationshipIdentifier,
      identifierDirection),
    stereotype) =>
      stringWrap(from.identifier) <+>
      opt(sourceMultiplicity, (s:String) => surround(text(s),'"')) <>
      showRelationshipConnector(relationshipType,relationshipDirection) <+>
      opt(targetMultiplicity, (s:String) => surround(text(s),'"')) <>
      stringWrap(to.identifier) <+>
        (if(relationshipIdentifier.isDefined || r.stereotype.isDefined) {
        ":" <+>
          (if (identifierDirection.equals(ToFrom)) {
            "<" <> space
          } else {
            emptyDoc
          }) <>
          (if (r.stereotype.isDefined) {
            '"'
          } else {
            emptyDoc
          }) <> showStereotype(r) <>
          opt(relationshipIdentifier, text) <>
          (if (stereotype.isDefined) {
            '"'
          } else {
            emptyDoc
          }) <>
          (if (identifierDirection.equals(FromTo)) {
            space <> ">" <> space
          } else {
            emptyDoc
          })
      } else {
        emptyDoc
      })
  }

  def opt[T](opt:Option[T],show:T => Doc,l:Doc=emptyDoc,r:Doc=space,emptyL:Doc = emptyDoc,emptyR:Doc = emptyDoc):Doc =
    opt.map(t => l <> show(t) <> r).getOrElse(emptyL <> emptyDoc <> emptyR)



  /**
   * Encloses a string with string literals if the string is more complex than a simple word.
   *
   * PlantUML allows identifier of objects to contain spaces and newline characters, if so the
   * identifier has to be wrapped as a string.
   *
   * @param text text to be wrapped
   * @return `text` wrapped in string literals if `text` contains spaces
   */
  def stringWrap(text:String):Doc = if (text.contains(" ")) surround(text,'"') else text

  def showStereotype(stereotypeElement: StereotypeElement):Doc =
    opt(stereotypeElement.stereotype,text,"<<" <> space,space <> ">>")


  def showAccessModifier(accessModifier: AccessModifier):String = accessModifier match {
    case Private => "-"
    case Protected => "#"
    case PackagePrivate => "~"
    case Public => "+"
  }

  def showModificators(modificators: List[Modificator]):String = modificators match {
    case Static :: mods => " {static}" + showModificators(mods)
    case Abstract :: mods => " {abstract}" + showModificators(mods)
    case Nil => ""
  }



  def showRelationshipConnector(relationshipType: RelationshipType,relationshipDirection: RelationshipDirection):String = relationshipType match {
    case Extension => appendRelationshipEnd("<|","--",relationshipDirection)
    case Composition => appendRelationshipEnd("*","--",relationshipDirection)
    case Aggregation => appendRelationshipEnd("o","--",relationshipDirection)
    case Annotation => appendRelationshipEnd("<","..",relationshipDirection)
    case Association => appendRelationshipEnd("<","--",relationshipDirection)
    case Inner => appendRelationshipEnd("+","--",relationshipDirection)
  }

  def appendRelationshipEnd(relEnd:String,relationship:String,relationshipDirection: RelationshipDirection):String = relationshipDirection match {
    case FromTo => relationship + inverse(relEnd)
    case ToFrom => relEnd + relationship
    case Without => relationship
  }

  def inverse(s:String): String = s match {
    case "<|" => "|>"
    case "<" => ">"
    case "*" => "*"
    case "o" => "o"
    case "+" => "+"
  }

}
