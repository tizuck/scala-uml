package scalameta.util.namespaces.collector
import scalameta.util.namespaces.{DefaultNamespace, Entry, NamespaceEmpty, NamespaceEntry}

import scala.meta.{Source, Stat}
import cats.implicits._

case class SourcesCollector(override val resultingMap: Map[Entry, List[Stat]])
  extends BaseNamespaceCollector

object SourcesCollector {
  def apply(sources:List[Source]): SourcesCollector = {
    SourcesCollector(sources.foldLeft(Map.empty[Entry,List[Stat]]){
      case (acc,source) =>
        val sourceMap = SourceCollector(source)
        acc |+| sourceMap.resultingMap
    }.removed(NamespaceEmpty)
      .map(tp => tp._1 match {case NamespaceEntry(List("default"),_) => DefaultNamespace -> tp._2 case _ => tp})
    )
  }
}
