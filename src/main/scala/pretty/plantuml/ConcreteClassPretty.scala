package pretty.plantuml

import pretty.Pretty
import pretty.config.PrettyConfig
import uml.{ConcreteClass, NamedElement, RelateableElement}
import pretty.Pretty._
import scalameta.util.namespaces.{DefaultNamespace, NamespaceEntry}

case class ConcreteClassPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[ConcreteClass] {
  override def toDoc(umlElement: ConcreteClass): Pretty.Doc = umlElement match {
    case ConcreteClass(cls) =>
      showNamespace(cls.namespace) <>
        (showNamespaceDot(cls.namespace)) <>
        cls.identifier
  }
}
