package scalameta.stats.defn

import scalameta.mods.ClassModsCollector
import scalameta.operations.PrimaryConstructorCollector
import scalameta.stats.StatsCollector
import scalameta.stats.init.InitsCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import scalameta.util.util.statToString
import uml.{Attribute, Class, ClassRef, Compartment, ConcreteClass, Inner, Operation, Parameter, Relationship, RelationshipInfo, ToFrom, UMLElement, Without}

import scala.meta.Defn


class DefnClassCollector(override val definedElements: List[UMLElement],
                         override val resultingContext: CollectorContext)
  extends BaseCollector

object DefnClassCollector {
  def apply(defnClass:Defn.Class)(implicit context : CollectorContext): DefnClassCollector = {

    val mods = ClassModsCollector(defnClass.mods)
    val className = defnClass.name.value

    val tempThisPointer = ClassRef(className,namespace = context.localCon.currentNamespace)
    val previousThisPointer = context.localCon.thisPointer
    val inheritedElements = InitsCollector(defnClass.templ.inits)(
      context.withThisPointer(tempThisPointer)
    )
    val previousToplevel = inheritedElements.resultingContext.localCon.isTopLevel
    val innerElements = StatsCollector(defnClass.templ.stats)(inheritedElements.resultingContext.notToplevel)
    val operations = innerElements.definedElements.flatMap{
      case o:Operation => Some(o)
      case _ => None
    }
    val primaryConstructor = PrimaryConstructorCollector(defnClass.ctor)(
      context.withCstrOrigin(className)
    )

    val cls = Class(
      mods.isAbstract,
      className,
      innerElements.attributes,
      primaryConstructor.primaryCstr.map(p => List(p)).getOrElse(Nil) ++ operations,
      mods.mods,
      None,
      mods.classStereotypes,
      context.localCon.currentNamespace
    )

    val innerRelationship = if(previousThisPointer.isDefined){
      Some(Relationship(Inner,ToFrom,RelationshipInfo(None,None,previousThisPointer.get,ConcreteClass(cls),None,Without),Nil))
    } else {None}

    new DefnClassCollector(
      cls ::
        innerElements.innerElements ++
          inheritedElements.definedElements ++
          innerRelationship.map(r => List(r)).getOrElse(Nil),
      innerElements
        .resultingContext
        .withOptionalThisPointer(previousThisPointer)
        .withToplevel(previousToplevel)
    )
  }


}

