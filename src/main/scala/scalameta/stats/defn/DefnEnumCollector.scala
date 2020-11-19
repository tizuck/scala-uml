package scalameta.stats.defn

import scalameta.mods.ClassModsCollector
import scalameta.operations.PrimaryConstructorCollector
import scalameta.stats.StatsCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Class, ClassRef, Stereotype, UMLElement}

import scala.meta.Defn

case class DefnEnumCollector(override val definedElements: List[UMLElement],
                             override val resultingContext: CollectorContext) extends BaseCollector

object DefnEnumCollector {
  def apply(defnEnum:Defn.Enum)(implicit context:CollectorContext): DefnEnumCollector = {
    val mods = ClassModsCollector(defnEnum.mods)
    val enumName = defnEnum.name

    val tempThisPointer = ClassRef(enumName.value,namespace = context.localCon.currentNamespace)
    val previousThisPointer = context.localCon.thisPointer
    val previousToplevel = context.localCon.isTopLevel
    val innerElements = StatsCollector(defnEnum.templ.stats)(context.withThisPointer(tempThisPointer).notToplevel)
    val primaryConstructor = PrimaryConstructorCollector(defnEnum.ctor)(
      innerElements
        .resultingContext
        .withCstrOrigin(enumName.value)
    )

    val cls = Class(
      true,
      enumName.value,
      Nil,
      primaryConstructor.primaryCstr.map(List(_)).getOrElse(Nil),
      Nil,
      None,
      List(Stereotype("scalaenum",Nil)),
      context.localCon.currentNamespace
    )

    DefnEnumCollector(
      cls :: innerElements.definedElements,
      innerElements
        .resultingContext
        .withOptionalThisPointer(previousThisPointer)
        .withToplevel(previousToplevel)
    )
  }
}
