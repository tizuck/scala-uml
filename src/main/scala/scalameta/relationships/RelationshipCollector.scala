package scalameta.relationships

import plantuml.Relationship
import scalameta.CollectorContext

import scala.meta.{Decl, Defn}

case class RelationshipCollector(relationships:List[Relationship])

object RelationshipCollector {
  def apply(defn:Defn)(implicit context:CollectorContext): RelationshipCollector = defn match {
    case Defn.Trait(_, name, _ , _ , template) =>
      val relationships = for(stat <- template.stats) yield {
        stat match {
          case decl: Decl =>
            DclRelationshipCollector(decl)(context.copy(thisPointer = name.value)).relationships
        }
      }
      new RelationshipCollector(relationships.flatten)
  }
}
