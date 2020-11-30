package pretty.plantuml

import pretty.Pretty
import pretty.config.PrettyConfig
import uml.TaggedValue
import pretty.Pretty._

case class TaggedValuePretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[TaggedValue] {
  override def toDoc(umlElement: TaggedValue): Pretty.Doc = umlElement match {
    case TaggedValue(name, value) =>
      name <>
        opt(value,text,"=")
  }
}
