package scalameta.toplevel

import plantuml.{Class, Relationship, TopLevelElement}
import scalameta.CollectorContext
import scalameta.common.TraitCollector
import scalameta.relationships.RelationshipCollector

import scala.meta.Defn

case class DefnToplevelCollector(topLevelElement: TopLevelElement, relationships:List[Relationship])

object DefnToplevelCollector {
  def apply(defn: Defn)(implicit context:CollectorContext) : DefnToplevelCollector = defn match {
    case d : Defn.Trait => {
      //@todo implement ClassBodyCollector
      val traitCollector = TraitCollector(d)
      new DefnToplevelCollector(traitCollector.pTrait,traitCollector.relationships)
    }
  }
}
