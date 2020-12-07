package pretty.plantuml

import pretty.KiamaPretty
import pretty.config.PrettyConfig
import uml.GenericParameter
import pretty.KiamaPretty._

case class GenericParameterPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[GenericParameter] {
  override def toDoc(umlElement: GenericParameter): KiamaPretty.Doc = umlElement match {
    case GenericParameter(identifier,concreteType,stereotype) =>
      showStereotype(stereotype) <>
        identifier <>
        opt(concreteType,text,space <> ":" <> space)
  }
}
