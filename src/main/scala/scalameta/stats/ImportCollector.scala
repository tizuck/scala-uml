package scalameta.stats

import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
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

  }
}
