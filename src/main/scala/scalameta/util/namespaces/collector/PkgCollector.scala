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
    println(s"in pkg ${pkg.name} with resultmap: ${(statsNamespaces + (NamespaceEmpty -> List(pkg))).map( tp => (tp._1,tp._2.map(statToString)))} ")
    PkgCollector(statsNamespaces + (NamespaceEmpty -> List(pkg)))
  }
}
