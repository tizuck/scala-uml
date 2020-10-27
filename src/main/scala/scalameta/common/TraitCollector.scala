package scalameta.common

import plantuml.{Class, Relationship}
import scalameta.CollectorContext
import scalameta.operations.{OperationCollector, PrimaryConstructorCollector}
import scalameta.relationships.RelationshipCollector

import scala.meta.Defn

case class TraitCollector(pTrait:Class, relationships:List[Relationship])

object TraitCollector {
  def apply(sTrait:Defn.Trait)(implicit context: CollectorContext): TraitCollector = {
    val relationships = RelationshipCollector(sTrait).relationships
    //@todo implement generic type parameter collector
    val traitName = sTrait.name.value

    val operations = OperationCollector(sTrait.templ.stats).operations

    val primaryConstructor = PrimaryConstructorCollector(sTrait.ctor)(context.copy(cstrOrigin = Some(traitName)))

    val cls = Class(
      true,
      traitName,
      List.empty ++
        primaryConstructor.primaryCstr.map(p => List(p)).getOrElse(Nil) ++
        operations,
      None,
      None)(Some("trait"))

    new TraitCollector(cls,relationships)
  }
}
