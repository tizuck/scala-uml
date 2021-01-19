package pretty.plantuml

import pretty.PrettyPrinter
import pretty.config.PrettyConfig
import uml.UMLUnit
import pretty.KiamaPretty._

case class UMLUnitPretty()(override implicit val config: PrettyConfig)
  extends PlantUMLPrettyPrinter[UMLUnit] {

  override def toDoc(umlElement: UMLUnit): Doc = {
    println(umlElement.toplevelElements.map(_.structure))
    "@startuml" <+>
      umlElement.name <@>
      vsep(umlElement.toplevelElements.map(TopLevelPretty().toDoc)) <@>
      "@enduml"
  }
}
