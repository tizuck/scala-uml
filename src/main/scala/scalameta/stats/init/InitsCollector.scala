package scalameta.stats.init

import scalameta.util.{BaseCollector, StateChangingCollector}
import scalameta.util.context.CollectorContext
import uml.{Relationship, UMLElement}

import scala.meta.Init

case class InitsCollector(override val resultingContext: CollectorContext,
                          override val definedElements: List[UMLElement]) extends BaseCollector

object InitsCollector {
  def apply(inits:List[Init])(implicit context : CollectorContext): InitsCollector =  {
      inits.foldLeft(InitsCollector(context,Nil)){
        case (acc,init) =>
          val initCol = InitCollector(init)(acc.resultingContext)
          acc.copy(initCol.resultingContext,definedElements = acc.definedElements ++ initCol.definedElements)
      }
  }

}
