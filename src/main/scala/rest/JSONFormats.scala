package rest

import spray.json._
import DefaultJsonProtocol._
import scalameta.util.namespaces
import scalameta.util.namespaces.{DefaultNamespace, Entry, Name, NamespaceEmpty, NamespaceEntry, TargetType, Wildcard}

import java.util.Base64
import scala.meta.Stat

object JSONFormats {

  implicit val scalaFileEntryJson: RootJsonFormat[ScalaFileEntry] =
    jsonFormat2(ScalaFileEntry)
  implicit val scalaFilesCollectionJson: RootJsonFormat[ScalaFilesCollection] =
    jsonFormat1(ScalaFilesCollection)

  import ast._

  implicit val astCreatedJson: RootJsonFormat[ASTCreated] =
    jsonFormat1(ASTCreated)

  object ast {
    import uml._

    /**
     * Top Level Element
     */
    implicit val topLevelFormat: RootJsonFormat[TopLevelElement] = new RootJsonFormat[TopLevelElement] {

      override def read(json: JsValue): TopLevelElement =
        json.asJsObject.getFields("type") match {
          case Seq(JsString("Package")) => json.convertTo[Package]
          case Seq(JsString("ClassDefRef")) => json.convertTo[externalReferences.ClassDefRef]
          case Seq(JsString("Class")) => json.convertTo[Class]
          case Seq(JsString("Relationship")) => json.convertTo[Relationship]
        }

      override def write(obj: TopLevelElement): JsValue = {
        obj match {
          case p:Package => p.toJson
          case c:externalReferences.ClassDefRef => c.toJson
          case c:Class => c.toJson
          case r:Relationship => r.toJson
        }
      }
    }


    implicit val targetTypeFormat : RootJsonFormat[TargetType] = new RootJsonFormat[TargetType] {
      override def read(json: JsValue): TargetType =
        json.asJsObject.getFields("type") match {
          case Seq(JsString("Wildcard")) => Wildcard
          case Seq(JsString("Name")) => Name
          case Seq(JsString("Package")) => scalameta.util.namespaces.Package
        }

      override def write(obj: TargetType): JsValue = JsObject(
        obj match {
            case Wildcard => "type" -> JsString("Wildcard")
            case Name => "type" -> JsString("Name")
            case namespaces.Package => "type" -> JsString("Package")
          }
      )
    }

    implicit val namespaceEntryFormat: RootJsonFormat[NamespaceEntry] = new RootJsonFormat[NamespaceEntry] {
      override def read(json: JsValue): NamespaceEntry = throw new NotImplementedError()

      override def write(obj: NamespaceEntry): JsValue = JsObject(
        Map(
          "qualifiers" -> obj.qualifiers.toJson,
          "targetType" -> obj.targetType.toJson,
          "type" -> JsString("NamespaceEntry")
        )
      )
    }

    implicit val entryFormat : RootJsonFormat[Entry] =  new RootJsonFormat[Entry] {
      override def read(json: JsValue): Entry = json.asJsObject.getFields("type") match {
        case Seq(JsString("NamespaceEmpty")) => NamespaceEmpty
        case Seq(JsString("DefaultNamespace")) => DefaultNamespace
        case Seq(JsString("NamespaceEntry")) => json.convertTo[NamespaceEntry]
      }


      override def write(obj: Entry): JsValue = JsObject(obj match {
        case NamespaceEmpty => Map("type" -> JsString("NamespaceEmpty"))
        case DefaultNamespace => Map("type" -> JsString("DefaultNamespace"))
        case n: NamespaceEntry => n.toJson.asJsObject.fields + ("type" -> JsString("NamespaceEntry"))
      })
    }

    implicit val classTypeFormat : RootJsonFormat[externalReferences.ClassType] =
      new RootJsonFormat[externalReferences.ClassType] {

      override def read(json: JsValue): externalReferences.ClassType = json.asJsObject.getFields("type") match {
        case Seq(JsString("Trait")) => externalReferences.Trait
        case Seq(JsString("Enum")) => externalReferences.Enum
        case Seq(JsString("Object")) => externalReferences.Object
        case Seq(JsString("CClass")) => externalReferences.CClass
        case Seq(JsString("CCaseClass")) => externalReferences.CCaseClass
      }

      override def write(obj: externalReferences.ClassType): JsValue = JsObject(
          obj match {
            case externalReferences.Trait => "type" -> JsString("Trait")
            case externalReferences.Enum =>"type" ->  JsString("Enum")
            case externalReferences.Object =>"type" ->  JsString("Object")
            case externalReferences.CClass =>"type" -> JsString("CClass")
            case externalReferences.CCaseClass => "type" -> JsString("CCaseClass")
          })
    }

    implicit val umlUnitJson: RootJsonFormat[UMLUnit] = new RootJsonFormat[UMLUnit] {
      override def read(json: JsValue): UMLUnit = throw new NotImplementedError()

      override def write(obj: UMLUnit): JsValue = {
        JsObject(
          Map(
            "name" -> obj.name.toJson,
            "toplevelElements" -> obj.toplevelElements.toJson,
            "type" -> JsString("UMLUnit")
          )
        )
      }
    }

    implicit val packageJson: RootJsonFormat[Package] = new RootJsonFormat[Package] {
      override def read(json: JsValue): Package = throw new NotImplementedError()

      override def write(obj: Package): JsValue = {
        JsObject(
          Map(
            "packageBodyElements" -> obj.packageBodyElements.toJson,
            "stereotype" -> obj.stereotype.toJson,
            "namespace" -> obj.namespace.toJson,
            "type" -> JsString("Package")
          )
        )
      }
    }

    /**
     * The JSON representation in here is only ment to send the content of a stat
     * in scalameta to the server as an encoded Base64 string, the encoding can be
     * done on frontend side.
     */
    object scalametaJSON {
      implicit val statFormat : RootJsonFormat[Stat] = new RootJsonFormat[Stat] {
        override def read(json: JsValue): Stat =
          throw new NotImplementedError("This method should never be invoked since " +
            "AST's are only included in a server response")

        override def write(obj: Stat): JsValue =  JsObject(
          Map(
            "contentBase64" -> JsString(Base64.getEncoder.encodeToString(obj.structure.getBytes)),
            "type" -> JsString("Scalameta.Stat")
          )
        )
      }
    }

    import scalametaJSON._
    implicit val classDefRefJson: RootJsonFormat[externalReferences.ClassDefRef] = jsonFormat5(externalReferences.ClassDefRef)

    implicit val modificatorFormat: RootJsonFormat[Modificator] = new RootJsonFormat[Modificator] {
      override def read(json: JsValue): Modificator =
        throw new NotImplementedError()

      override def write(obj: Modificator): JsValue =
          obj match {
            case Static => JsObject(Map("type" -> JsString("Static")))
            case Abstract => JsObject(Map("type" -> JsString("Abstract")))
          }
    }

    implicit val accessModifierFormat: RootJsonFormat[AccessModifier] = new RootJsonFormat[AccessModifier] {
      override def read(json: JsValue): AccessModifier =
        throw new NotImplementedError()

      override def write(obj: AccessModifier): JsValue = JsObject(
        obj match {
          case Private => "type" -> JsString("Private")
          case Protected => "type" -> JsString("Protected")
          case PackagePrivate => "type" -> JsString("PackagePrivate")
          case Public => "type" -> JsString("Public")
        }
      )
    }

    implicit val taggedValueFormat : RootJsonFormat[TaggedValue] = new RootJsonFormat[TaggedValue] {
      override def read(json: JsValue): TaggedValue = throw new NotImplementedError()

      override def write(obj: TaggedValue): JsValue = JsObject(
        Map(
          "name" -> obj.name.toJson,
          "value" -> obj.value.toJson,
          "type" -> JsString("TaggedValue")
        )
      )
    }

    implicit val parameterFormat : RootJsonFormat[Parameter] = new RootJsonFormat[Parameter] {
      override def read(json: JsValue): Parameter = throw new NotImplementedError()

      override def write(obj: Parameter): JsValue = JsObject(
        Map(
          "name" -> obj.name.toJson,
          "paramType" -> obj.paramType.toJson,
          "stereotype" -> obj.stereotype.toJson,
          "type" -> JsString("Parameter")
        )
      )
    }

    implicit val genericParameterFormat : RootJsonFormat[GenericParameter] = new RootJsonFormat[GenericParameter] {
      override def read(json: JsValue): GenericParameter = throw new NotImplementedError()

      override def write(obj: GenericParameter): JsValue = JsObject(
        Map(
          "name" -> obj.name.toJson,
          "concreteType" -> obj.concreteType.toJson,
          "stereotype" -> obj.stereotype.toJson,
          "type" -> JsString("GenericParameter")
        )
      )
    }

    implicit val operationFormat : RootJsonFormat[Operation] = new RootJsonFormat[Operation] {
      override def read(json: JsValue): Operation = throw new NotImplementedError()

      override def write(obj: Operation): JsValue = JsObject(
        Map(
          "modificator" -> obj.modificator.toJson,
          "accessModifier" -> obj.accessModifier.toJson,
          "name" -> obj.name.toJson,
          "paramSeq" -> obj.paramSeq.toJson,
          "returnType" -> obj.returnType.toJson,
          "stereotype" -> obj.stereotype.toJson,
          "templateParameter" -> obj.templateParameter.toJson,
          "type" -> JsString("Operation")
        )
      )
    }

    implicit val attributeFormat: RootJsonFormat[Attribute] = new RootJsonFormat[Attribute] {
      override def read(json: JsValue): Attribute = throw new NotImplementedError()

      override def write(obj: Attribute): JsValue = JsObject(
        Map(
          "modificators" -> obj.modificators.toJson,
          "modifier" -> obj.modifier.toJson,
          "name" -> obj.name.toJson,
          "attributeType" -> obj.attributeType.toJson,
          "stereotype" -> obj.stereotype.toJson,
          "defaultValue" -> obj.defaultValue.toJson,
          "type" -> JsString("Attribute")
        )
      )
    }

    implicit val compartmentFormat : RootJsonFormat[Compartment] = new RootJsonFormat[Compartment] {
      override def read(json: JsValue): Compartment = throw new NotImplementedError()

      override def write(obj: Compartment): JsValue = JsObject(
        Map(
          "identifier" -> obj.identifier.toJson,
          "taggedValues" -> obj.taggedValues.toJson,
          "stereotype" -> obj.stereotype.toJson,
          "type" -> JsString("Compartment")
        )
      )
    }

    implicit val classJson: RootJsonFormat[Class] = new RootJsonFormat[Class] {
      override def read(json: JsValue): Class = throw new NotImplementedError()

      override def write(obj: Class): JsValue = JsObject(
        Map(
          "isAbstract" -> obj.isAbstract.toJson,
          "name" -> obj.name.toJson,
          "attributes" -> obj.attributes.toJson,
          "operations" -> obj.operations.toJson,
          "additionalCompartements" -> obj.additionalCompartements.toJson,
          "genericParameters" -> obj.genericParameters.toJson,
          "stereotype" -> obj.stereotype.toJson,
          "type" -> JsString("Class")
        )
      )
    }

    implicit val relationshipTypeFormat : RootJsonFormat[RelationshipType] = new RootJsonFormat[RelationshipType] {
      override def read(json: JsValue): RelationshipType =
        throw new NotImplementedError()

      override def write(obj: RelationshipType): JsValue = JsObject(
        obj match {
          case Extension => "type" -> JsString("Extension")
          case Realization => "type" -> JsString("Realization")
          case Composition => "type" -> JsString("Composition")
          case Aggregation => "type" -> JsString("Aggregation")
          case Annotation => "type" -> JsString("Annotation")
          case Association => "type" -> JsString("Association")
          case Inner => "type" -> JsString("Inner")
        }
      )
    }

    implicit val relationshipDirectionFormat: RootJsonFormat[RelationshipDirection] = new RootJsonFormat[RelationshipDirection] {
      override def read(json: JsValue): RelationshipDirection =
        throw new NotImplementedError()

      override def write(obj: RelationshipDirection): JsValue = JsObject(
        obj match {
          case FromTo => "type" -> JsString("FromTo")
          case ToFrom => "type" -> JsString("ToFrom")
          case Without => "type" -> JsString("Without")
        }
      )
    }

    implicit val concreteClassFormat: RootJsonFormat[ConcreteClass] = new RootJsonFormat[ConcreteClass] {
      override def read(json: JsValue): ConcreteClass = throw new NotImplementedError()

      override def write(obj: ConcreteClass): JsValue = JsObject(
        Map(
          "class" -> obj.cls.toJson,
          "type" -> JsString("ConcreteClass")
        )
      )
    }

    implicit val classRefFormat: RootJsonFormat[ClassRef] = new RootJsonFormat[ClassRef] {
      override def read(json: JsValue): ClassRef = throw new NotImplementedError()

      override def write(obj: ClassRef): JsValue = JsObject(
        Map(
          "name" -> obj.name.toJson,
          "namespace" -> obj.namespace.toJson,
          "type" -> JsString("ClassRef")
        )
      )
    }

    /**
     * Automatic generation causes runtime error
     */
    implicit val packageRef: RootJsonFormat[PackageRef] = new RootJsonFormat[PackageRef] {
      override def read(json: JsValue): PackageRef =
        throw new NotImplementedError()

      override def write(obj: PackageRef): JsValue = JsObject (
        obj.namespace.toJson.asJsObject.fields + ("type" -> JsString("PackageRef"))
      )
    }

    implicit val relationshipElementFormat : RootJsonFormat[RelationshipElement] = new RootJsonFormat[RelationshipElement] {
      override def read(json: JsValue): RelationshipElement =
        throw new NotImplementedError()

      override def write(obj: RelationshipElement): JsValue = obj match {
        case c:ConcreteClass => c.toJson
        case c:ClassRef => c.toJson
        case p:PackageRef => p.toJson
      }
    }

    implicit val relationshipInfoFormat : RootJsonFormat[RelationshipInfo] = new RootJsonFormat[RelationshipInfo] {
      override def read(json: JsValue): RelationshipInfo = throw new NotImplementedError()

      override def write(obj: RelationshipInfo): JsValue = JsObject(
        Map(
          "sourceMultiplicity" -> obj.sourceMultiplicity.toJson,
          "targetMultiplicity" -> obj.targetMultiplicity.toJson,
          "from" -> obj.from.toJson,
          "to" -> obj.to.toJson,
          "relationshipIdentifier" -> obj.relationshipIdentifier.toJson,
          "identifierDirection" -> obj.identifierDirection.toJson,
          "originType" -> obj.originType.toJson,
          "type" -> JsString("RelationshipInfo")
        )
      )
    }

    implicit val releationshipJson: RootJsonFormat[Relationship] = new RootJsonFormat[Relationship] {
      override def read(json: JsValue): Relationship = throw new NotImplementedError()

      override def write(obj: Relationship): JsValue = JsObject(
        Map(
          "relationshipType" -> obj.relationshipType.toJson,
          "relationshipDirection" -> obj.relationshipDirection.toJson,
          "relationshipInfo" -> obj.relationshipInfo.toJson,
          "stereotype" -> obj.stereotype.toJson,
          "type" -> JsString("Relationship")
        )
      )
    }

    /**
     * Package Body Element
     */
    implicit val packageBodyElementFormat : RootJsonFormat[PackageBodyElement] =
      new RootJsonFormat[PackageBodyElement] {

      override def read(json: JsValue): PackageBodyElement = json.asJsObject.getFields("type") match {
        case Seq(JsString("Package")) => json.convertTo[Package]
        case Seq(JsString("Class")) => json.convertTo[Class]
        case Seq(JsString("Relationship")) => json.convertTo[Relationship]
      }

      override def write(obj: PackageBodyElement): JsValue = obj match {
          case p:Package => p.toJson
          case c:Class => c.toJson
          case r:Relationship => r.toJson
      }
    }

    implicit val stereotypeFormat: RootJsonFormat[Stereotype] = new RootJsonFormat[Stereotype] {
      override def read(json: JsValue): Stereotype = throw new NotImplementedError()

      override def write(obj: Stereotype): JsValue = JsObject(
        Map(
          "name" -> obj.name.toJson,
          "taggedValues" -> obj.taggedValues.toJson,
          "type" -> JsString("Stereotype")
        )
      )
    }

    private def appendType[T](conv:Map[String,JsValue],t:T): Map[String, JsValue] =
      conv + ("type" -> JsString(t.getClass.getSimpleName))
  }
}
