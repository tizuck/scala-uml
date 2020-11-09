package scalameta.util.namespaces.collector
import scalameta.util.namespaces.{Entry, NamespaceEmpty}

import scala.meta.{Defn, Stat}

case class TraitCollector(override val resultingMap: Map[Entry, List[Stat]])
  extends BaseNamespaceCollector

object TraitCollector {
  def apply(trit:Defn.Trait): TraitCollector = {
    TraitCollector(
      Map[Entry,List[Stat]](
        NamespaceEmpty -> List(trit)
      )
    )
  }
}
