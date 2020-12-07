package pretty.plantuml

import pretty.KiamaPretty
import pretty.config.PrettyConfig
import uml.Attribute
import pretty.KiamaPretty._

case class AttributePretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[Attribute] {
  override def toDoc(umlElement: Attribute): KiamaPretty.Doc = umlElement match {
    case Attribute(
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
  }
}
