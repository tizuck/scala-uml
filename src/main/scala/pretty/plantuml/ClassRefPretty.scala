package pretty.plantuml

import pretty.KiamaPretty
import pretty.config.PrettyConfig
import uml.ClassRef
import pretty.KiamaPretty._

case class ClassRefPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[ClassRef] {
  override def toDoc(umlElement: ClassRef): KiamaPretty.Doc = umlElement match {
    case ClassRef(name,namespace) =>
      showNamespace(namespace) <>
        showNamespaceDot(namespace)<>
        name
  }
}

