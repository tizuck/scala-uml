package scalameta.toplevel


import scalameta.{StateChangingCollector, CollectorContext}
import scalameta.common.TraitCollector
import uml.{Relationship, TopLevelElement, Class}

import scala.meta.Defn

case class DefnToplevelCollector(topLevelElement: TopLevelElement,
                                 relationships:List[Relationship],
                                 typeClasses:List[Class],
                                 override val resultingContext: CollectorContext) extends StateChangingCollector

object DefnToplevelCollector {
  def apply(defn: Defn)(implicit context:CollectorContext) : DefnToplevelCollector = defn match {
    case d : Defn.Trait => {
      val traitCollector = TraitCollector(d)
      new DefnToplevelCollector(traitCollector.pTrait,
        traitCollector.relationships,
        traitCollector.typeClasses,
        traitCollector.resultingContext.copy(thisPointer = None))
    }
  }
}
