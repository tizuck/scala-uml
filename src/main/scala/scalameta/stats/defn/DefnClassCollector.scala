package scalameta.stats.defn

import scalameta.mods.ClassModsCollector
import scalameta.operations.PrimaryConstructorCollector
import scalameta.stats.StatsCollector
import scalameta.stats.init.InitsCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import scalameta.util.util.statToString
import uml.{Attribute, Class, ClassRef, Compartment, ConcreteClass, Inner, Operation, Relationship, RelationshipInfo, ToFrom, UMLElement, Without}

import scala.meta.Defn


class DefnClassCollector(override val definedElements: List[UMLElement],
                         override val resultingContext: CollectorContext)
  extends BaseCollector

object DefnClassCollector {
  def apply(defnClass:Defn.Class)(implicit context : CollectorContext): DefnClassCollector = {
    println(s"imports for visit of ${statToString(defnClass)}: " + context.localCon.currentImports + "globalcontext:" + context.globalCon.globalScope.map{
      case (k,v) => (k,v.map(statToString))
    })
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
      if(mods.modifier.nonEmpty) {Compartment(Some("<<ScalaClass>>"),mods.modifier,None) :: Nil} else Nil,
      None,
      mods.stereotype,
      context.localCon.currentNamespace
    )

    val innerRelationship = if(previousThisPointer.isDefined){
      Some(Relationship(Inner,ToFrom,RelationshipInfo(None,None,previousThisPointer.get,ConcreteClass(cls),None,Without),None))
    } else {None}

    new DefnClassCollector(
      cls ::
        innerElements.innerElements ++
          inheritedElements.definedElements ++
          innerRelationship.map(r => List(r)).getOrElse(Nil),
      innerElements
        .resultingContext
        .withAdditionalTemplate(cls)
        .withOptionalThisPointer(previousThisPointer)
        .withToplevel(previousToplevel)
    )
  }


}

