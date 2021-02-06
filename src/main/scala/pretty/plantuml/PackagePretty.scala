package pretty.plantuml

import pretty.config.PrettyConfig
import pretty.KiamaPretty._

case class PackagePretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[uml.Package] {

  override def toDoc(umlElement: uml.Package): Doc = umlElement match {
    case uml.Package(packageBodyElements, stereotype, namespace) =>
      "package" <+>
        showStereotype(stereotype) <>
        showNamespace(namespace) <>
        enclose("{",nest(line <> vsep(packageBodyElements.map(PackageBodyPretty().toDoc))),line <> "}")
  }
}
