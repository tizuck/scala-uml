package pretty.plantuml

import pretty.KiamaPretty
import pretty.config.PrettyConfig
import uml.{ClassRef, ConcreteClass, PackageRef, RelationshipElement}

case class RelationshipElementPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[RelationshipElement] {
  override def toDoc(umlElement: RelationshipElement): KiamaPretty.Doc = umlElement match {
    case c:ConcreteClass => ConcreteClassPretty().toDoc(c)
    case c:ClassRef => ClassRefPretty().toDoc(c)
    case p:PackageRef=> PackageRefPretty().toDoc(p)
  }
}
