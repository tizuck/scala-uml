package scalameta.util.namespaces.collector
import scalameta.util.namespaces.Entry

import scala.meta.{ Source, Stat}

case class SourceCollector(val resultingMap: Map[Entry, List[(Stat,String)]])

object SourceCollector {
  def apply(source:Source)(implicit compilationUnitName : String): SourceCollector = {
    new SourceCollector(
      StatsCollector(source.stats,None)
        .resultingMap
        .map(
          entry => (entry._1, entry._2.map(stat => (stat,compilationUnitName)))
        )
    )
  }
}
