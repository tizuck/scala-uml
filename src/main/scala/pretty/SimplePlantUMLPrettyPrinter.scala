/*
 * Copyright 2015 Tilman Zuckmantel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package pretty

import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
import java.util.UUID.randomUUID

import scalameta.util.namespaces.DefaultNamespace
import uml.externalReferences.{ClassDefRef, Trait}
import uml.{Abstract, AccessModifier, Aggregation, Annotation, Association, Attribute, Class, ClassRef, Compartment, Composition, ConcreteClass, Extension, FromTo, GenericParameter, Inner, Modificator, Note, Operation, Package, PackagePrivate, Parameter, Private, Protected, Public, Relationship, RelationshipDirection, RelationshipInfo, RelationshipType, Static, Stereotype, StereotypeElement, TaggedValue, ToFrom, UMLElement, UMLUnit, Without}

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
    packageBodyElements,stereotype,namespace) =>
      "package" <+>
        showStereotype(stereotype) <>
        namespace.plantUML <>
        enclose("{",nest(line <> vsep(packageBodyElements.map(show))),line <> "}")

    case GenericParameter(
    identifier,
    concreteType,stereotype) =>
      showStereotype(stereotype) <>
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
        stringWrap(namespace.plantUML + name) <>
        opt(genericParameter,  (gps:List[GenericParameter]) => hsep(gps.map(show),sep = ','),l="< ",r=" >" <> space,emptyR = space) <>
        showStereotype(stereotype) <>
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
    attributeType,
    stereotype,
    defaultValue) =>
      opt(accessModifier,showAccessModifier,r=emptyDoc) <>
        showStereotype(stereotype) <>
        opt(modificators,showModificators) <>
        identifier <+>
        opt(attributeType,text,':' <> space) <>
        opt(defaultValue,text,"=")

    case p@Parameter(
    identifier,
    paramType,stereotype) =>
      showStereotype(stereotype) <>
        identifier <+>
        ':' <+>
        paramType

    case o@Operation(
    modificators,
    accessModifier,
    identifier,
    paramSeq,
    returnType,
    stereotype,
    templateParameter) =>
      showStereotype(stereotype) <>
        opt(modificators,showModificators) <>
        opt(accessModifier,showAccessModifier) <>
        identifier <>
        opt(templateParameter,  (gps:List[GenericParameter]) => hsep(gps.map(show),sep = ','),l="< ",r=" >" <> space,emptyR = space) <>
        hsep(paramSeq.map(params => '(' <> hsep(params.map(show),", ") <> ')')) <+>
        opt(returnType,text,":" <> space,r = emptyDoc)

    case Compartment(identifier, compartmentElements,_) =>
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

    case Stereotype(name,taggedValues) =>
      name <>
        (if(taggedValues.nonEmpty) space <> hsep(taggedValues.map(show),",")else emptyDoc)


    case TaggedValue(name, value) =>
      name <>
        opt(value,text,"=")

    case ConcreteClass(cls) =>
      cls.namespace.plantUML <> cls.name

    case ClassRef(name,namespace) =>
      namespace.plantUML <> name

    case ClassDefRef(classtype, name, namespace, templateParameter, _) =>
      classtype match {
        case Trait =>
          val cls = Class(true, name, Nil, Nil, Nil,
            Option.when(templateParameter.nonEmpty)(templateParameter.map(s => GenericParameter(s,None,Nil))),
            List(Stereotype("trait",Nil)), namespace
          )
          show(cls)
        case uml.externalReferences.Enum =>
          val cls = Class(false, name, Nil, Nil, Nil,
            Option.when(templateParameter.nonEmpty)(templateParameter.map(s => GenericParameter(s,None,Nil))),
            List(Stereotype("enum",Nil)), namespace
          )
          show(cls)
        case uml.externalReferences.Object =>
          val cls = Class(false, name, Nil, Nil, Nil, None, List(Stereotype("object",Nil)), namespace)
          show(cls)
        case uml.externalReferences.CClass =>
          val cls = Class(false, name, Nil, Nil, Nil,
            Option.when(templateParameter.nonEmpty)(templateParameter.map(s => GenericParameter(s,None,Nil))),
            Nil, namespace
        )
          show(cls)
        case uml.externalReferences.CCaseClass =>
          val cls = Class(false, name, Nil, Nil, Nil,
            Option.when(templateParameter.nonEmpty)(templateParameter.map(s => GenericParameter(s,None,Nil))),
            List(Stereotype("caseclass",Nil)), namespace)
          show(cls)
      }

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
        (if(relationshipIdentifier.isDefined || stereotype.nonEmpty) {
        ":" <+>
          (if (identifierDirection.equals(ToFrom)) {
            "<" <> space
          } else {
            emptyDoc
          }) <>
           showStereotype(stereotype) <>
          opt(relationshipIdentifier, text)  <>
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

  def showStereotype(stereotypes:List[Stereotype]): SimplePlantUMLPrettyPrinter.Doc = {
    if(stereotypes.nonEmpty) {
      "<<" <+> hsep(stereotypes.map(show),",") <+> ">>"
    } else emptyDoc
  }


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
