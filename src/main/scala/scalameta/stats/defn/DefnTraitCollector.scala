package scalameta.stats.defn

import scalameta.mods.ClassModsCollector
import scalameta.operations.PrimaryConstructorCollector
import scalameta.stats.StatsCollector
import scalameta.stats.init.InitsCollector
import scalameta.typeparams.TypeParamsCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Class, ClassRef, Compartment, ConcreteClass, Inner, Operation, Relationship, RelationshipInfo, Stereotype, ToFrom, UMLElement, Without}

import scala.meta.Defn

case class DefnTraitCollector(override val definedElements : List[UMLElement],
                              override val resultingContext: CollectorContext
                                     )
  extends BaseCollector

object DefnTraitCollector {
  def apply(defnTrait:Defn.Trait)(implicit context:CollectorContext): DefnTraitCollector = {


    val traitName = defnTrait.name.value
    val mods = ClassModsCollector(defnTrait.mods)
    println(s"Trait:$traitName with mods: $mods")
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

    if(defnTrait.name.value.equals("StereotypeElement")){
      println(innerElements.definedElements)
    }
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
      mods.mods,
      genericParameter,
      List(Stereotype("trait",Nil)),
      context.localCon.currentNamespace
    )

    val innerRelationship = previousThisPointer.flatMap( r =>
      Some(Relationship(Inner,ToFrom,RelationshipInfo(None,None,r,ConcreteClass(cls),None,Without),Nil))
    )

    new DefnTraitCollector(
      cls :: innerElements.innerElements ++ inheritedElements.definedElements ++ innerRelationship.map(r => List(r)).getOrElse(Nil),
      innerElements
        .resultingContext
        .withOptionalThisPointer(previousThisPointer)
        .withToplevel(previousToplevel)
    )
  }
}
