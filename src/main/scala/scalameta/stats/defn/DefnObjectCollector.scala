package scalameta.stats.defn

import scalameta.mods.ObjectModsCollector
import scalameta.stats.StatsCollector
import scalameta.stats.init.InitsCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Attribute, Class, ClassRef, Compartment, ConcreteClass, Inner, Operation, Relationship, RelationshipInfo, ToFrom, UMLElement, Without}

import scala.meta.Defn

case class DefnObjectCollector(override val definedElements: List[UMLElement],
                               override val resultingContext: CollectorContext) extends BaseCollector

object DefnObjectCollector {
  def apply(defnObject:Defn.Object)(implicit context:CollectorContext): DefnObjectCollector = {
    val mods = ObjectModsCollector(defnObject.mods)
    val objectName = defnObject.name.value

    val tempThisPointer = ClassRef(objectName)
    val previousThisPointer = context.localCon.thisPointer
    val previousToplevel = context.localCon.isTopLevel

    val inheritedElements = InitsCollector(defnObject.templ.inits)(context.withThisPointer(tempThisPointer))
    val innerElements = StatsCollector(defnObject.templ.stats)(inheritedElements.resultingContext.notToplevel)
    val operations = innerElements.definedElements.flatMap{
      case o:Operation => Some(o)
      case _ => None
    }
    val innerWithoutOperations = innerElements.definedElements.flatMap{
      case _:Operation => None
      case other => Some(other)
    }

    val cls = Class(
      false,
      objectName,
      innerWithoutOperations.flatMap{case a:Attribute => Some(a) case _ => None},
      operations,
      if(mods.modifiers.nonEmpty) {Compartment(Some("<<ScalaClass>>"),mods.modifiers,None) :: Nil} else Nil,
      None,
      mods.stereotype.orElse(Some("object"))
    )

    val innerRelationship = if(previousThisPointer.isDefined){
      Some(Relationship(Inner,ToFrom,RelationshipInfo(None,None,previousThisPointer.get,ConcreteClass(cls),None,Without),None))
    } else {None}

    new DefnObjectCollector(
      cls :: innerWithoutOperations ++ inheritedElements.inheritance ++ innerRelationship.map( List(_)).getOrElse(Nil),
      innerElements
        .resultingContext
        .withAdditionalTemplate(cls)
        .withOptionalThisPointer(previousThisPointer)
        .withToplevel(previousToplevel)
    )
  }
}
