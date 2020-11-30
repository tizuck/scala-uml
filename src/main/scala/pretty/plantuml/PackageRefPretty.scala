package pretty.plantuml

import pretty.Pretty
import pretty.config.PrettyConfig
import uml.PackageRef

case class PackageRefPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[PackageRef] {
  override def toDoc(umlElement: PackageRef): Pretty.Doc = umlElement match {
    case PackageRef(namespace) => showNamespace(namespace)
  }
}
