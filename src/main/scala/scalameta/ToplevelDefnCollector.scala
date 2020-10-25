package scalameta

import plantuml.{Class, ClassBodyElement, Relationship, TopLevelElement, UMLElement}
import scalameta.relationships.RelationshipCollector

import scala.meta.Defn

case class ToplevelDefnCollector(topLevelElement: TopLevelElement, relationships:List[Relationship])

object ToplevelDefnCollector {
  def apply(defn: Defn)(implicit context:CollectorContext) : ToplevelDefnCollector = defn match {
    case Defn.Trait(mods, name, value1, primary, template) => {
      val relationships = RelationshipCollector(defn).relationships
      //@todo implement generic type parameter collector
      //@todo here
      println("here")
      println(relationships)
      val traitName = name.value

      //@todo implement ClassBodyCollector
      new ToplevelDefnCollector(Class(true,traitName,Seq.empty,None,None)(Some("trait")),relationships)
    }
  }
}
