package scalameta.stats.defn

import scalameta.mods.ClassModsCollector
import scalameta.operations.PrimaryConstructorCollector
import scalameta.stats.init.InitsCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Class, ClassRef, UMLElement}

import scala.meta.Defn

case class DefnEnumCaseCollector(override val definedElements: List[UMLElement],
                                 override val resultingContext: CollectorContext)  extends BaseCollector

object DefnEnumCaseCollector {
  def apply(defnEnumCase:Defn.EnumCase)(implicit context:CollectorContext): DefnEnumCaseCollector = {
    val mods = ClassModsCollector(defnEnumCase.mods)
    val caseName = defnEnumCase.name.value

    val tempThisPointer = ClassRef(caseName,namespace = context.localCon.currentNamespace)
    val previousThisPointer = context.localCon.thisPointer

    val inheritedElements = InitsCollector(defnEnumCase.inits)(
      context.withThisPointer(tempThisPointer)
    )
    val primaryConstructor = PrimaryConstructorCollector(defnEnumCase.ctor)(
      inheritedElements.resultingContext.withCstrOrigin(caseName)
    )

    val cls = Class(
      false,
      caseName,
      Nil,
      primaryConstructor.primaryCstr.map(List(_)).getOrElse(Nil),
      Nil,
      None,
      Some("case"),
      context.localCon.currentNamespace
    )

    new DefnEnumCaseCollector(
      cls :: inheritedElements.inheritance,
      inheritedElements.resultingContext.copy(
        context.localCon.copy(thisPointer = previousThisPointer,
        definedTemplates = cls :: inheritedElements.resultingContext.localCon.definedTemplates)
      )
    )
  }
}
