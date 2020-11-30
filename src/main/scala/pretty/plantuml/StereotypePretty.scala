package pretty.plantuml

import pretty.Pretty
import pretty.config.PrettyConfig
import uml.Stereotype
import pretty.Pretty._

case class StereotypePretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[Stereotype] {
  override def toDoc(umlElement: Stereotype): Pretty.Doc = umlElement match {
    case Stereotype(name,taggedValues) =>
      name <>
        (if(taggedValues.nonEmpty) space <> hsep(taggedValues.map(TaggedValuePretty().toDoc),",")else emptyDoc)
  }
}
