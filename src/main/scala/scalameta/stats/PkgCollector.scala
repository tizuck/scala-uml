package scalameta.stats

import scalameta.util.{BaseCollector, StateChangingCollector}
import scalameta.util.context.CollectorContext
import scalameta.util.namespaces.collector.BaseNamespaceCollector.qualName
import scalameta.util.namespaces.{DefaultNamespace, NamespaceEmpty, NamespaceEntry}
import uml.UMLElement

import scala.meta.Pkg

case class PkgCollector(override val resultingContext: CollectorContext,
                        override val definedElements: List[UMLElement])
  extends BaseCollector

object PkgCollector {
  /**
   * Updates the current namespace of the local context and sets it
   * according to the package reference. Visits all nodes contained
   * in `pkg.stats` with the updated namespace
   *
   * @param pkg visited node that contains namespace information
   * @param context context of previous node visiting
   * @return all collected elements of `pkg.stats` in `definedElements` and
   *         a context with the previous namespace and
   *         updated local context information
   */
  def apply(pkg: Pkg)(implicit context: CollectorContext): PkgCollector = {
    val oldNamespace = context.localCon.currentNamespace
    val newNamespace = oldNamespace match {
      case DefaultNamespace => qualName(pkg.ref)
      case NamespaceEntry(qualifiers) => NamespaceEntry(qualifiers ++ qualName(pkg.ref).qualifiers)
      case _ => throw new IllegalStateException(s"Unexpected empty namespace in pkg: ${pkg.ref}")
    }
    val innerstats = StatsCollector(pkg.stats)(context.withNamespace(newNamespace))
    PkgCollector(innerstats.resultingContext.withNamespace(oldNamespace),innerstats.definedElements)
  }
}

