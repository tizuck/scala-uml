package scalameta.stats.defn

import scalameta.mods.ClassModsCollector
import scalameta.operations.PrimaryConstructorCollector
import scalameta.stats.StatsCollector
import scalameta.stats.init.InitsCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Attribute, Class, ClassRef, Compartment, ConcreteClass, Inner, Operation, Relationship, RelationshipInfo, ToFrom, UMLElement, Without}

import scala.meta.Defn


class DefnClassCollector(override val definedElements: List[UMLElement],
                         override val resultingContext: CollectorContext)
  extends BaseCollector

object DefnClassCollector {
  def apply(defnClass:Defn.Class)(implicit context : CollectorContext): DefnClassCollector = {
    val mods = ClassModsCollector(defnClass.mods)
    val className = defnClass.name.value
    println(s"for class:${className} mods: ${mods.modifier} with original modifiers: ${defnClass.mods}")
    val tempThisPointer = Some(ClassRef(className))
    val previousThisPointer = context.localCon.thisPointer
    val inheritedElements = InitsCollector(defnClass.templ.inits)(
      context.copy(context.localCon.copy(thisPointer = tempThisPointer))
    )
    val innerElements = StatsCollector(defnClass.templ.stats)(inheritedElements.resultingContext)
    val operations = innerElements.definedElements.flatMap{
      case o:Operation => Some(o)
      case _ => None
    }
    val innerWithoutOperations = innerElements.definedElements.flatMap{
      case _:Operation => None
      case other => Some(other)
    }
    val primaryConstructor = PrimaryConstructorCollector(defnClass.ctor)(
      context.copy(context.localCon.copy(cstrOrigin = Some(className)))
    )

    val cls = Class(
      mods.isAbstract,
      className,
      innerWithoutOperations.flatMap{case a:Attribute => Some(a) case _ => None},
      primaryConstructor.primaryCstr.map(p => List(p)).getOrElse(Nil) ++ operations,
      if(mods.modifier.nonEmpty) {Compartment(Some("<<ScalaClass>>"),mods.modifier,None) :: Nil} else Nil,
      None,
      mods.stereotype
    )

    val innerRelationship = if(previousThisPointer.isDefined){
      Some(Relationship(Inner,ToFrom,RelationshipInfo(None,None,previousThisPointer.get,ConcreteClass(cls),None,Without),None))
    } else {None}

    new DefnClassCollector(
      cls ::
        innerWithoutOperations ++
          inheritedElements.inheritance ++
          innerRelationship.map(r => List(r)).getOrElse(Nil),
      innerElements.resultingContext.copy( innerElements.resultingContext.localCon.copy(
        definedTemplates  = cls :: innerElements.resultingContext.localCon.definedTemplates,
        thisPointer = previousThisPointer
      ))
    )
  }


}

