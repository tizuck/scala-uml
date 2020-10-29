package scalameta.stats.defn

import scalameta.common.RelationshipBaseCollector
import scalameta.operations.{PrimaryConstructorCollector}
import scalameta.{CollectorContext, StatsCollector}
import uml.{Class, Operation, UMLElement}

import scala.meta.Defn

case class DefnTraitRelationshipCollector(override val definedElements : List[UMLElement],
                                          override val resultingContext: CollectorContext
                                     )
  extends RelationshipBaseCollector

object DefnTraitRelationshipCollector {
  def apply(defnTrait:Defn.Trait)(implicit context:CollectorContext): DefnTraitRelationshipCollector = {
    //@todo implement generic type parameter collector
    val traitName = defnTrait.name.value

    val innerElements = StatsCollector(defnTrait.templ.stats)(context.copy(thisPointer = Some(Class(true,traitName,Nil,Nil,Nil,None,None))))

    val operations = innerElements.definedElements.flatMap{
      case o:Operation =>Some(o)
      case _ => None
    }

    val innerWithoutOperations = innerElements.definedElements.flatMap{
      case _:Operation => None
      case other => Some(other)
    }

    val primaryConstructor = PrimaryConstructorCollector(defnTrait.ctor)(context.copy(cstrOrigin = Some(traitName)))

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

    new DefnTraitRelationshipCollector(
      cls :: innerWithoutOperations,
      innerElements.resultingContext.copy(definedTemplates = cls :: innerElements.resultingContext.definedTemplates)
    )
  }
}
