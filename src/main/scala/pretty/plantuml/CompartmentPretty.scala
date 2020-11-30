package pretty.plantuml

import pretty.Pretty
import pretty.config.PrettyConfig
import uml.Compartment
import pretty.Pretty._

case class CompartmentPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[Compartment] {
  override def toDoc(umlElement: Compartment): Pretty.Doc = umlElement match {
    case Compartment(identifier, taggedValues, stereotype) =>
      "--" <>
        opt(identifier,text,l=space,r=space <> "--") <>
        nest (
          line <>
            vsep(taggedValues.map(TaggedValuePretty().toDoc))
        )
  }
}
