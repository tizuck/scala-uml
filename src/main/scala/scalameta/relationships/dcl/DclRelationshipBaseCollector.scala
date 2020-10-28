package scalameta.relationships.dcl

import scalameta.{CollectorContext, StateChangingCollector}
import uml.{Relationship,Class}

trait DclRelationshipBaseCollector extends StateChangingCollector {
  val relationships : List[Relationship]
  val resultingContext : CollectorContext
  val typeClass : Option[Class]
}
