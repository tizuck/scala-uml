package scalameta.stats.init

import scalameta.util.{CollectorContext, StateChangingCollector}
import uml.Relationship

import scala.meta.{Init}

case class InitsCollector(override val resultingContext: CollectorContext,
                          inheritance:List[Relationship]) extends StateChangingCollector

object InitsCollector {
  def apply(inits:List[Init])(implicit context : CollectorContext): InitsCollector =  {
      inits.foldLeft(InitsCollector(context,Nil)){
        case (acc,init) =>
          val initCol = InitCollector(init)(acc.resultingContext)
          acc.copy(initCol.resultingContext,inheritance = acc.inheritance ++ List(initCol.inheritance))
      }
  }

}
