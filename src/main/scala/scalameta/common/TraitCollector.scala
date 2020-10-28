package scalameta.common


import scalameta.{StateChangingCollector, CollectorContext}
import scalameta.operations.{OperationCollector, PrimaryConstructorCollector}
import scalameta.relationships.RelationshipCollector
import uml.{Class, Relationship}

import scala.meta.Defn

case class TraitCollector(pTrait:Class,
                          relationships:List[Relationship],
                          typeClasses:List[Class],
                          override val resultingContext: CollectorContext) extends StateChangingCollector

object TraitCollector {
  def apply(sTrait:Defn.Trait)(implicit context: CollectorContext): TraitCollector = {
    //@todo implement generic type parameter collector
    val traitName = sTrait.name.value

    val operations = OperationCollector(sTrait.templ.stats).operations

    val primaryConstructor = PrimaryConstructorCollector(sTrait.ctor)(context.copy(cstrOrigin = Some(traitName)))

    val cls = Class(
      true,
      traitName,
      List.empty,
      List.empty ++
        primaryConstructor.primaryCstr.map(p => List(p)).getOrElse(Nil) ++
        operations,
      List.empty,
      None,
      Some("trait"))

    val relationships = RelationshipCollector(sTrait)(
      context.copy(thisPointer = Some(cls),definedTemplates = cls :: context.definedTemplates.filter(_.identifier.equals(traitName)))
    )

    new TraitCollector(cls,relationships.relationships,relationships.typeClasses,relationships.resultingContext)
  }
}
