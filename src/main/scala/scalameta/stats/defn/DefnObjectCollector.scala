package scalameta.stats.defn

import scalameta.mods.ObjectModsCollector
import scalameta.stats.StatsCollector
import scalameta.stats.init.InitsCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Attribute, Class, Compartment, Inner, Operation, Relationship, RelationshipInfo, ToFrom, UMLElement, Without}

import scala.meta.Defn

case class DefnObjectCollector(override val definedElements: List[UMLElement],
                               override val resultingContext: CollectorContext) extends BaseCollector

object DefnObjectCollector {
  def apply(defnObject:Defn.Object)(implicit context:CollectorContext): DefnObjectCollector = {
    val mods = ObjectModsCollector(defnObject.mods)
    val objectName = defnObject.name.value

    val tempThisPointer = Some(Class(true,objectName,Nil,Nil,Nil,None,None))
    val previousThisPointer = context.thisPointer

    val inheritedElements = InitsCollector(defnObject.templ.inits)(context.copy(thisPointer = tempThisPointer))
    val innerElements = StatsCollector(defnObject.templ.stats)(inheritedElements.resultingContext)
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
      Some(Relationship(Inner,ToFrom,RelationshipInfo(None,None,previousThisPointer.get,cls,None,Without),None))
    } else {None}

    new DefnObjectCollector(
      cls :: innerWithoutOperations ++ inheritedElements.inheritance ++ innerRelationship.map( List(_)).getOrElse(Nil),
      innerElements.resultingContext.copy(
        definedTemplates =  cls :: innerElements.resultingContext.definedTemplates,
        thisPointer = previousThisPointer
      )
    )
  }
}
