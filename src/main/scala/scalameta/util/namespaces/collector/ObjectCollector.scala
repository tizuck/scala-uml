package scalameta.util.namespaces.collector
import scalameta.util.namespaces.{Entry, NamespaceEmpty}

import scala.meta.{Defn, Stat, Term}


case class ObjectCollector(override val resultingMap: Map[Entry, List[Stat]])
  extends BaseNamespaceCollector

object ObjectCollector {
  def apply(obj:Defn.Object): ObjectCollector = {
    val statsNamespaces = StatsCollector(
      obj.templ.stats,Some(BaseNamespaceCollector.qualName(Term.Name(obj.name.value)))
    )
    ObjectCollector(
      statsNamespaces.resultingMap + (NamespaceEmpty -> List(obj))
    )
  }
}
