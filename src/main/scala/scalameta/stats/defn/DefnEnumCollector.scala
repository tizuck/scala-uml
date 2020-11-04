package scalameta.stats.defn

import scalameta.mods.ClassModsCollector
import scalameta.operations.PrimaryConstructorCollector
import scalameta.stats.StatsCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Class, ClassRef, UMLElement}

import scala.meta.Defn

case class DefnEnumCollector(override val definedElements: List[UMLElement],
                             override val resultingContext: CollectorContext) extends BaseCollector

object DefnEnumCollector {
  def apply(defnEnum:Defn.Enum)(implicit context:CollectorContext): DefnEnumCollector = {
    val mods = ClassModsCollector(defnEnum.mods)
    val enumName = defnEnum.name

    val tempThisPointer = Some(ClassRef(enumName.value))
    val previousThisPointer = context.localCon.thisPointer

    val innerElements = StatsCollector(defnEnum.templ.stats)(context.copy(context.localCon.copy(thisPointer = tempThisPointer)))
    val primaryConstructor = PrimaryConstructorCollector(defnEnum.ctor)(
      innerElements.resultingContext.copy(innerElements.resultingContext.localCon.copy(cstrOrigin = Some(enumName.value)))
    )

    val cls = Class(
      true,
      enumName.value,
      Nil,
      primaryConstructor.primaryCstr.map(List(_)).getOrElse(Nil),
      Nil,
      None,
      Some("scalaenum")
    )

    DefnEnumCollector(
      cls :: innerElements.definedElements,
      innerElements.resultingContext.copy(innerElements.resultingContext.localCon.copy(thisPointer = previousThisPointer))
    )
  }
}
