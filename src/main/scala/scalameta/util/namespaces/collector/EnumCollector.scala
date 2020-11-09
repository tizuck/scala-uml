package scalameta.util.namespaces.collector
import scalameta.util.namespaces.{Entry, NamespaceEmpty}

import scala.meta.{Defn, Stat}

case class EnumCollector(override val resultingMap: Map[Entry, List[Stat]])
  extends BaseNamespaceCollector

object EnumCollector {
  def apply(enum:Defn.Enum): EnumCollector = {
    EnumCollector(
      Map[Entry,List[Stat]](
        NamespaceEmpty -> List(enum)
      ))
  }
}
