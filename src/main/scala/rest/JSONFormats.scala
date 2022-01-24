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
        JsObject(appendType((obj match {
          case p:Package => p.toJson
          case c:externalReferences.ClassDefRef => c.toJson
          case c:Class => c.toJson
          case r:Relationship => r.toJson
        }).asJsObject.fields, obj))
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
    implicit val namespaceEntryFormat: RootJsonFormat[NamespaceEntry] = jsonFormat2(NamespaceEntry)

    implicit val namespaceEmptyFormat : RootJsonFormat[Entry] =  new RootJsonFormat[Entry] {
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

    implicit val umlUnitJson: RootJsonFormat[UMLUnit] = jsonFormat2(UMLUnit)
    implicit val packageJson: RootJsonFormat[Package] = jsonFormat3(Package)

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
            "type" -> JsString("Scalameta.Stat"),
            "contentBase64" -> JsString(Base64.getEncoder.encodeToString(obj.structure.getBytes))
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

    implicit val taggedValueFormat : RootJsonFormat[TaggedValue] = jsonFormat2(TaggedValue)

    implicit val parameterFormat : RootJsonFormat[Parameter] = jsonFormat3(Parameter)
    implicit val genericParameterFormat : RootJsonFormat[GenericParameter] = jsonFormat3(GenericParameter)

    implicit val operationFormat : RootJsonFormat[Operation] = jsonFormat7(Operation)
    implicit val attributeFormat: RootJsonFormat[Attribute] = jsonFormat6(Attribute)

    implicit val compartmentFormat : RootJsonFormat[Compartment] = jsonFormat3(Compartment)

    implicit val classJson: RootJsonFormat[Class] = jsonFormat8(Class)

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

    implicit val relationshipElementFormat : RootJsonFormat[RelationshipElement] = new RootJsonFormat[RelationshipElement] {
      override def read(json: JsValue): RelationshipElement =
        throw new NotImplementedError()

      implicit val concreteClassFormat: RootJsonFormat[ConcreteClass] = jsonFormat1(ConcreteClass)
      implicit val classRefFormat: RootJsonFormat[ClassRef] = jsonFormat2(ClassRef)

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

      override def write(obj: RelationshipElement): JsValue = JsObject(
        appendType((obj match {
          case c:ConcreteClass => c.toJson
          case c:ClassRef => c.toJson
          case p:PackageRef => p.toJson
        }).asJsObject.fields,obj)
      )
    }

    implicit val relationshipInfoFormat : RootJsonFormat[RelationshipInfo] = jsonFormat7(RelationshipInfo)

    implicit val releationshipJson: RootJsonFormat[Relationship] = jsonFormat4(Relationship)

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

      override def write(obj: PackageBodyElement): JsValue = {
        JsObject(appendType((obj match {
          case p:Package => p.toJson
          case c:Class => c.toJson
          case r:Relationship => r.toJson
        }).asJsObject.fields,obj))
      }
    }

    implicit val stereotypeFormat: RootJsonFormat[Stereotype] = jsonFormat2(Stereotype)

    private def appendType[T](conv:Map[String,JsValue],t:T): Map[String, JsValue] =
      conv + ("type" -> JsString(t.getClass.getSimpleName))
  }
}
