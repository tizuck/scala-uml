package pretty.plantuml

import pretty.KiamaPretty
import pretty.config.PrettyConfig
import uml.Parameter
import pretty.KiamaPretty._

case class ParameterPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[Parameter] {
  override def toDoc(umlElement: Parameter): KiamaPretty.Doc = umlElement match {
    case Parameter(identifier, paramType, stereotype) =>
      showStereotype(stereotype) <>
        identifier <+>
        ':' <+>
        paramType
  }
}
