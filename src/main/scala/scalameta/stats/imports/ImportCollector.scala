package scalameta.stats.imports

import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import scalameta.util.namespaces.NamespaceEntry
import uml.UMLElement

import scala.meta.Import

case class ImportCollector(override val definedElements: List[UMLElement],
                           override val resultingContext: CollectorContext) extends BaseCollector

object ImportCollector {
  /**
   * Builds imported `NamespaceEntry` from structure of `imprt`
   * @param imprt
   * @param context
   * @return
   */
  def apply(imprt:Import)(implicit context:CollectorContext): ImportCollector = {
    val namespaces = imprt.importers.foldLeft(List.empty[NamespaceEntry]){
      case (acc,importer) => acc ++ ImporterCollector(importer).namespaces
    }
    //println(s"imports: ${context.withAdditionalImports(namespaces).localCon}")
    ImportCollector(Nil,context.withAdditionalImports(namespaces))
  }
}
