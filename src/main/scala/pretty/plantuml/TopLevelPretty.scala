package pretty.plantuml

import pretty.PrettyPrinter
import pretty.config.PrettyConfig
import uml.externalReferences.ClassDefRef
import uml.{Note, Relationship, TopLevelElement, externalReferences}
import pretty.Pretty._

case class TopLevelPretty()(override implicit val config: PrettyConfig) extends PlantUMLPrettyPrinter[TopLevelElement] {

  override def toDoc(umlElement: TopLevelElement): Doc = umlElement match {
    case p:uml.Package  =>  PackagePretty().toDoc(p)
    case c:ClassDefRef  =>  ClassDefRefPretty().toDoc(c)
    case c:uml.Class    =>  ClassPretty().toDoc(c)
    case n:Note         =>  NotePretty().toDoc(n)
    case r:Relationship =>  RelationshipPretty().toDoc(r)
  }
}
