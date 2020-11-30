package pretty.plantuml

import pretty.PrettyPrinter
import pretty.config.PrettyConfig
import pretty.Pretty._

case class PackagePretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[uml.Package] {

  override def toDoc(umlElement: uml.Package): Doc = umlElement match {
    case uml.Package(packageBodyElements, stereotype, namespace) =>
      "package" <+>
        showStereotype(stereotype) <>
        namespace.plantUML <>
        enclose("{",nest(line <> vsep(packageBodyElements.map(PackageBodyPretty().toDoc))),line <> "}")
  }
}
