package scalameta.util.namespaces.collector
import scalameta.util.namespaces.{Entry, NamespaceEmpty}

import scala.meta.{Defn, Stat}

case class ClassCollector(override val resultingMap: Map[Entry, List[Stat]])
  extends BaseNamespaceCollector

object ClassCollector {
  def apply(cls:Defn.Class): ClassCollector = {
    ClassCollector(Map[Entry,List[Stat]](
      NamespaceEmpty -> List(cls)
    ))
  }
}
