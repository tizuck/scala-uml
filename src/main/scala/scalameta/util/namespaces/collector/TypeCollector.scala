package scalameta.util.namespaces.collector
import scalameta.util.namespaces.{Entry, NamespaceEmpty}

import scala.meta.{Defn, Stat}

case class TypeCollector(override val resultingMap: Map[Entry, List[Stat]])
  extends BaseNamespaceCollector

object TypeCollector {
  def apply(tpe:Defn.Type): TypeCollector = {
    TypeCollector(Map[Entry,List[Stat]](NamespaceEmpty -> List(tpe)))
  }
}
