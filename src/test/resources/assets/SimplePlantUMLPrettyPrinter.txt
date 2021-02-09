package plantuml

import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
import java.util.UUID.randomUUID

import scalameta.util.namespaces.DefaultNamespace
import uml.{Abstract, AccessModifier, Aggregation, Annotation, Association, Attribute, Class, ClassRef, Compartment, Composition, ConcreteClass, Extension, FromTo, GenericParameter, Inner, Modificator, Note, Operation, Package, PackagePrivate, Parameter, Private, Protected, Public, Relationship, RelationshipDirection, RelationshipInfo, RelationshipType, Static, StereotypeElement, ToFrom, UMLElement, UMLUnit, Without}

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
    name,
    packageBodyElements,stereotype,namespace) =>
      "package" <+>
        (namespace match {
          case DefaultNamespace => stringWrap(stereotype.map(ns => s"<<$ns>> ").getOrElse(""))
          case ns@_ => stringWrap(s"${stereotype.map(ns => s"<<$ns>> ").getOrElse("")}$ns.$name")
        }) <+>
        enclose("{",nest(line <> vsep(packageBodyElements.map(show))),line <> "}")

    case GenericParameter(
    identifier,
    concreteType,stereotype) =>
      opt(stereotype,text,r = emptyDoc) <>
          identifier <>
          opt(concreteType,text,space <> ":" <> space)

    case Class(
    isAbstract,
    name: String,
    attributes,
    operations,
    additionalCompartements,
    genericParameter,
    stereotype,
    namespace) =>
      (if(isAbstract) {"abstract" <> space } else { emptyDoc }) <>
        "class" <+>
        (namespace match {case DefaultNamespace => name case s@_ => stringWrap(s + "." + name)}) <>
        opt(genericParameter,  (gps:List[GenericParameter]) => hsep(gps.map(show),sep = ','),l="< ",r=" >" <> space,emptyR = space) <>
        opt(stereotype,text,"<<" <> space,space <> ">>") <>
        (if(attributes.nonEmpty || operations.nonEmpty || additionalCompartements.nonEmpty) {
          enclose("{",
            nest(line <>
              vsep(attributes.map(show)) <>
              (if (attributes.nonEmpty) line else emptyDoc) <>
              vsep(operations.map(show))
            ) <> space <>
            (if (operations.nonEmpty) line else emptyDoc) <>
              vsep(additionalCompartements.map(show)) <> line,
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
        identifier <+> opt(attributeType,text,':' <> space)

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
          opt(identifier,text,l=space,r=space <> "--") <>
          nest (
            line <>
            vsep(compartmentElements.map(show))
          )

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

    case ConcreteClass(cls) =>
      cls.namespace match {case DefaultNamespace => cls.identifier case s@_ => stringWrap(s + "." + cls.identifier)}

    case ClassRef(name,namespace) =>
      namespace match {case DefaultNamespace => name case s@_ => stringWrap(s + "." + name)}

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
      show(from) <+>
      opt(sourceMultiplicity, (s:String) => surround(text(s),'"')) <>
      showRelationshipConnector(relationshipType,relationshipDirection) <+>
      opt(targetMultiplicity, (s:String) => surround(text(s),'"')) <>
      show(to) <+>
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
