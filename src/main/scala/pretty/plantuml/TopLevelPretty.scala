package pretty.plantuml

import pretty.config.PrettyConfig
import pretty.KiamaPretty._
import uml.externalReferences.ClassDefRef
import uml.{Relationship, TopLevelElement}

case class TopLevelPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[TopLevelElement] {

  override def toDoc(umlElement: TopLevelElement): Doc = umlElement match {
    case p:uml.Package  =>  PackagePretty().toDoc(p)
    case c:ClassDefRef  =>  ClassDefRefPretty().toDoc(c)
    case c:uml.Class    =>  ClassPretty().toDoc(c)
    case r:Relationship =>  RelationshipPretty().toDoc(r)
  }
}
