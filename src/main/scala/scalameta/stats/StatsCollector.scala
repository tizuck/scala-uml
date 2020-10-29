package scalameta.stats

import scalameta.util.{BaseCollector, CollectorContext}
import uml.UMLElement

import scala.meta.Stat

case class StatsCollector(override val resultingContext: CollectorContext,
                          override val definedElements: List[UMLElement])
  extends BaseCollector

object StatsCollector {
  def apply(stats:List[Stat])(implicit context:CollectorContext): StatsCollector = {
    stats.foldLeft(StatsCollector(context,Nil)){
      case (acc,stat) =>
        val statCol = StatCollector(stat)(acc.resultingContext)
        acc.copy(statCol.resultingContext,definedElements = statCol.definedElements ++ acc.definedElements)
    }
  }
}
