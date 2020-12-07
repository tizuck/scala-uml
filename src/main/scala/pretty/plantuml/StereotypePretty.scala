package pretty.plantuml

import pretty.KiamaPretty
import pretty.config.PrettyConfig
import uml.Stereotype
import pretty.KiamaPretty._

case class StereotypePretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[Stereotype] {
  override def toDoc(umlElement: Stereotype): KiamaPretty.Doc = umlElement match {
    case Stereotype(name,taggedValues) =>
      name <>
        (if(taggedValues.nonEmpty) space <> hsep(taggedValues.map(TaggedValuePretty().toDoc),",")else emptyDoc)
  }
}
