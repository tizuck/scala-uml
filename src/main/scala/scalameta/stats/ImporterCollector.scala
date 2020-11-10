package scalameta.stats

import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.UMLElement

import scala.meta.Importer

case class ImporterCollector(override val definedElements: List[UMLElement],
                             override val resultingContext: CollectorContext) extends BaseCollector

object ImporterCollector {
  def apply(importer:Importer): ImporterCollector = {

  }
}
