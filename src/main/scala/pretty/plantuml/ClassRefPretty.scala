package pretty.plantuml

import pretty.Pretty
import pretty.config.PrettyConfig
import uml.ClassRef
import pretty.Pretty._

case class ClassRefPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[ClassRef] {
  override def toDoc(umlElement: ClassRef): Pretty.Doc = umlElement match {
    case ClassRef(name,namespace) =>
      showNamespace(namespace) <>
        showNamespaceDot(namespace)<>
        name
  }
}

