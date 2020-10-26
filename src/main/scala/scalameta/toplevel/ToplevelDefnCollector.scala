package scalameta.toplevel

import plantuml.{Class, Relationship, TopLevelElement}
import scalameta.CollectorContext
import scalameta.common.TraitCollector
import scalameta.relationships.RelationshipCollector

import scala.meta.Defn

case class ToplevelDefnCollector(topLevelElement: TopLevelElement, relationships:List[Relationship])

object ToplevelDefnCollector {
  def apply(defn: Defn)(implicit context:CollectorContext) : ToplevelDefnCollector = defn match {
    case d : Defn.Trait => {
      //@todo implement ClassBodyCollector
      val traitCollector = TraitCollector(d)
      new ToplevelDefnCollector(traitCollector.pTrait,traitCollector.relationships)
    }
  }
}
