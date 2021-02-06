package pretty.plantuml

import pretty.KiamaPretty
import pretty.config.PrettyConfig
import uml.Compartment
import pretty.KiamaPretty._

case class CompartmentPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[Compartment] {
  override def toDoc(umlElement: Compartment): KiamaPretty.Doc = umlElement match {
    case Compartment(identifier, taggedValues, _) =>
      "--" <>
        opt(identifier,text,l=space,r=space <> "--") <>
        nest (
          line <>
            vsep(taggedValues.map(TaggedValuePretty().toDoc))
        )
  }
}
