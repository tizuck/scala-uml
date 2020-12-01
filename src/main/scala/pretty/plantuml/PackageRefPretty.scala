package pretty.plantuml

import pretty.KiamaPretty
import pretty.config.PrettyConfig
import uml.PackageRef

case class PackageRefPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[PackageRef] {
  override def toDoc(umlElement: PackageRef): KiamaPretty.Doc = umlElement match {
    case PackageRef(namespace) => showNamespace(namespace)
  }
}
