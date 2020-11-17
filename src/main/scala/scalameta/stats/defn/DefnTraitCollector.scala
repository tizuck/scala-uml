package scalameta.stats.defn

import scalameta.operations.PrimaryConstructorCollector
import scalameta.stats.StatsCollector
import scalameta.stats.init.InitsCollector
import scalameta.typeparams.TypeParamsCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Class, ClassRef, ConcreteClass, Inner, Operation, Relationship, RelationshipInfo, ToFrom, UMLElement, Without}

import scala.meta.Defn

case class DefnTraitCollector(override val definedElements : List[UMLElement],
                              override val resultingContext: CollectorContext
                                     )
  extends BaseCollector

object DefnTraitCollector {
  def apply(defnTrait:Defn.Trait)(implicit context:CollectorContext): DefnTraitCollector = {

    val traitName = defnTrait.name.value
    val typeParameters = TypeParamsCollector(defnTrait.tparams).typeParams
    val genericParameter = Option.when(typeParameters.nonEmpty)(typeParameters)
    val tempThisPointer = ClassRef(traitName,namespace = context.localCon.currentNamespace)
    //Collect thisPointer for inner associations
    val previousThisPointer = context.localCon.thisPointer
    val inheritedElements = InitsCollector(defnTrait.templ.inits)(
      context.withThisPointer(tempThisPointer)
    )
    val previousToplevel = inheritedElements.resultingContext.localCon.isTopLevel
    val innerElements = StatsCollector(defnTrait.templ.stats)(inheritedElements.resultingContext.notToplevel)
    val primaryConstructor = PrimaryConstructorCollector(defnTrait.ctor)(
      context.withCstrOrigin(traitName)
    )
    val cls = Class(
      true,
      traitName,
      List.empty,
      List.empty ++
        primaryConstructor.primaryCstr.map(p => List(p)).getOrElse(Nil) ++
        innerElements.operations,
      List.empty,
      genericParameter,
      Some("trait"),
      context.localCon.currentNamespace
    )

    val innerRelationship = previousThisPointer.flatMap( r =>
      Some(Relationship(Inner,ToFrom,RelationshipInfo(None,None,r,ConcreteClass(cls),None,Without),None))
    )

    new DefnTraitCollector(
      innerElements.innerElements ++ inheritedElements.definedElements ++ innerRelationship.map(r => List(r)).getOrElse(Nil) ++ List(cls),
      innerElements
        .resultingContext
        .withOptionalThisPointer(previousThisPointer)
        .withToplevel(previousToplevel)
    )
  }
}
