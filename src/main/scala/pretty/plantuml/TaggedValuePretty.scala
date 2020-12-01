package pretty.plantuml

import pretty.KiamaPretty
import pretty.config.PrettyConfig
import uml.TaggedValue
import pretty.KiamaPretty._

case class TaggedValuePretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[TaggedValue] {
  override def toDoc(umlElement: TaggedValue): KiamaPretty.Doc = umlElement match {
    case TaggedValue(name, value) =>
      name <>
        opt(value,text,"=")
  }
}
