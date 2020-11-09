package scalameta.util.namespaces.collector
import scalameta.util.namespaces.Entry

import scala.meta.{ Source, Stat}

case class SourceCollector(override val resultingMap: Map[Entry, List[Stat]])
  extends BaseNamespaceCollector

object SourceCollector {
  def apply(source:Source): SourceCollector = {
    SourceCollector(
      StatsCollector(source.stats,None).resultingMap
    )
  }
}
