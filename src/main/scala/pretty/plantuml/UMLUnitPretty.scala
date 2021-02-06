package pretty.plantuml

import pretty.config.PrettyConfig
import pretty.KiamaPretty._
import uml.UMLUnit

case class UMLUnitPretty()(override implicit val config: PrettyConfig)
  extends PlantUMLPrettyPrinter[UMLUnit] {

  override def toDoc(umlElement: UMLUnit): Doc = {
    "@startuml" <+>
      umlElement.name <@>
      vsep(umlElement.toplevelElements.map(TopLevelPretty().toDoc)) <@>
      "@enduml"
  }
}
