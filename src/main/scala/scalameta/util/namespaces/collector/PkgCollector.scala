package scalameta.util.namespaces.collector

import scalameta.util.namespaces.{Entry, NamespaceEmpty, NamespaceEntry, collector}
import scalameta.util.util.statToString

import scala.meta.{Defn, Pkg, Stat}

case class PkgCollector(override val resultingMap: Map[Entry, List[Stat]])
  extends BaseNamespaceCollector

object PkgCollector {
  def apply(pkg:Pkg): PkgCollector = {
    val pkgNamespace = BaseNamespaceCollector.qualName(pkg.ref)
    val statsNamespaces = StatsCollector(pkg.stats,Some(pkgNamespace)).resultingMap
    PkgCollector(statsNamespaces + (NamespaceEmpty -> List(pkg)))
  }
}
