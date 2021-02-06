package pretty.plantuml

import pretty.PrettyPrinter
import scalameta.util.namespaces.{DefaultNamespace, Entry, NamespaceEmpty, NamespaceEntry}
import pretty.KiamaPretty._
import uml.{Abstract, AccessModifier, Aggregation, Annotation, Association, Composition, Extension, FromTo,
  Inner, Modificator, PackagePrivate, Private, Protected, Public, Realization, RelationshipDirection, RelationshipType,
  Static, Stereotype, ToFrom, UMLElement, Without}

trait PlantUMLPrettyPrinter[T <: UMLElement] extends PrettyPrinter[T] {

  override def showNamespace(e: Entry): Doc = e match {
    case NamespaceEmpty => ""
    case DefaultNamespace => ""
    case NamespaceEntry(qualifiers, _) =>
      ssep(qualifiers.map(text),"::")
  }

  protected[this] final def showStereotype(stereotypes:List[Stereotype]): Doc = {
    if(stereotypes.nonEmpty) {
      "<<" <+> hsep(stereotypes.map(StereotypePretty().toDoc),",") <+> ">>"
    } else emptyDoc
  }

  protected[this] final def showModificators(modificators: List[Modificator]):String = modificators match {
    case Static :: mods => " {static}" + showModificators(mods)
    case Abstract :: mods => " {abstract}" + showModificators(mods)
    case Nil => ""
  }

  protected[this] final def showNamespaceDot(namespace:scalameta.util.namespaces.Entry):Doc = {
    namespace match {
      case _: NamespaceEntry => "."
      case _ => emptyDoc
    }
  }

  protected[this] final def showRelationshipConnector(
                                                       relationshipType: RelationshipType,
                                                       relationshipDirection: RelationshipDirection)
  :String = relationshipType match {

    case Extension => appendRelationshipEnd("<|","--",relationshipDirection)
    case Composition => appendRelationshipEnd("*","--",relationshipDirection)
    case Aggregation => appendRelationshipEnd("o","--",relationshipDirection)
    case Annotation => appendRelationshipEnd("<","..",relationshipDirection)
    case Association => appendRelationshipEnd("<","--",relationshipDirection)
    case Inner => appendRelationshipEnd("+","--",relationshipDirection)
    case Realization => appendRelationshipEnd("<|","..",relationshipDirection)
  }

  protected[this] final def appendRelationshipEnd(
                                                   relEnd:String,
                                                   relationship:String,
                                                   relationshipDirection: RelationshipDirection)
  :String = relationshipDirection match {

    case FromTo => relationship + inverse(relEnd)
    case ToFrom => relEnd + relationship
    case Without => relationship
  }

  protected[this] final def inverse(s:String): String = s match {
    case "<|" => "|>"
    case "<" => ">"
    case "*" => "*"
    case "o" => "o"
    case "+" => "+"
  }

  override def showAccessModifier(accessModifier: AccessModifier):Doc = accessModifier match {
    case Private => "-"
    case Protected => "#"
    case PackagePrivate => "~"
    case Public => "+"
  }


}
