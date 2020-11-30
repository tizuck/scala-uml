package pretty.plantuml

import pretty.Pretty
import pretty.config.PrettyConfig
import uml.GenericParameter
import pretty.Pretty._

case class GenericParameterPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[GenericParameter] {
  override def toDoc(umlElement: GenericParameter): Pretty.Doc = umlElement match {
    case GenericParameter(identifier,concreteType,stereotype) =>
      showStereotype(stereotype) <>
        identifier <>
        opt(concreteType,text,space <> ":" <> space)
  }
}
