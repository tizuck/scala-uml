package scalameta.common

import plantuml.{Class, Relationship}
import scalameta.CollectorContext
import scalameta.operations.OperationCollector
import scalameta.relationships.RelationshipCollector

import scala.meta.Defn

case class TraitCollector(pTrait:Class, relationships:List[Relationship])

object TraitCollector {
  def apply(sTrait:Defn.Trait)(implicit context: CollectorContext): TraitCollector = {
    val relationships = RelationshipCollector(sTrait).relationships
    //@todo implement generic type parameter collector
    val traitName = sTrait.name.value

    val operations = OperationCollector(sTrait.templ.stats).operations

    new TraitCollector(Class(true,traitName,List.empty ++ operations,None,None)(Some("trait")),relationships)
  }
}
