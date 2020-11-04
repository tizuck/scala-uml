package scalameta.stats.defn

import scalameta.operations.PrimaryConstructorCollector
import scalameta.stats.StatsCollector
import scalameta.stats.init.InitsCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Class, Inner, Operation, Relationship, RelationshipInfo, ToFrom, UMLElement, Without}

import scala.meta.Defn

case class DefnTraitCollector(override val definedElements : List[UMLElement],
                              override val resultingContext: CollectorContext
                                     )
  extends BaseCollector

object DefnTraitCollector {
  def apply(defnTrait:Defn.Trait)(implicit context:CollectorContext): DefnTraitCollector = {
    //@todo implement generic type parameter collector
    val traitName = defnTrait.name.value

    val tempThisPointer = Class(true,traitName,Nil,Nil,Nil,None,None)
    //Collect thisPointer for inner associations
    val previousThisPointer = context.thisPointer

    val inheritedElements = InitsCollector(defnTrait.templ.inits)(context.copy(thisPointer = Some(tempThisPointer)))
    val innerElements = StatsCollector(defnTrait.templ.stats)(inheritedElements.resultingContext)

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

    val innerRelationship = if(previousThisPointer.isDefined){
      Some(Relationship(Inner,ToFrom,RelationshipInfo(None,None,previousThisPointer.get,cls,None,Without),None))
    } else {None}

    new DefnTraitCollector(
      cls :: innerWithoutOperations ++ inheritedElements.inheritance ++ innerRelationship.map(r => List(r)).getOrElse(Nil),
      innerElements.resultingContext.copy(definedTemplates = cls :: innerElements.resultingContext.definedTemplates,
        thisPointer = previousThisPointer)
    )
  }
}
