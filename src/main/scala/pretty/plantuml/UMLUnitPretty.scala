package pretty.plantuml

import pretty.PrettyPrinter
import pretty.config.PrettyConfig
import uml.UMLUnit
import pretty.Pretty._

case class UMLUnitPretty()(override implicit val config: PrettyConfig)
  extends PlantUMLPrettyPrinter[UMLUnit] {

  override def toDoc(umlElement: UMLUnit): Doc = {
    "@startuml" <+>
      umlElement.identifier <@>
      vsep(umlElement.toplevelElements.map(TopLevelPretty().toDoc)) <@>
      "@enduml"
  }
}
