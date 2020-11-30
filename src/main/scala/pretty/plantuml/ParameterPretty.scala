package pretty.plantuml

import pretty.Pretty
import pretty.config.PrettyConfig
import uml.Parameter
import pretty.Pretty._

case class ParameterPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[Parameter] {
  override def toDoc(umlElement: Parameter): Pretty.Doc = umlElement match {
    case Parameter(identifier, paramType, stereotype) =>
      showStereotype(stereotype) <>
        identifier <+>
        ':' <+>
        paramType
  }
}
