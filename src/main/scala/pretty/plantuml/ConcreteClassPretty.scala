package pretty.plantuml

import pretty.KiamaPretty
import pretty.config.PrettyConfig
import pretty.KiamaPretty._
import uml.ConcreteClass

case class ConcreteClassPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[ConcreteClass] {
  override def toDoc(umlElement: ConcreteClass): KiamaPretty.Doc = umlElement match {
    case ConcreteClass(cls) =>
      showNamespace(cls.namespace) <>
        showNamespaceDot(cls.namespace) <>
        cls.name
  }
}
