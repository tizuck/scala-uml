package scalameta

import scalameta.stats.StatsCollector
import scalameta.util.context.CollectorContext

import scala.meta.{Defn, Source}
import uml.{TopLevelElement, UMLUnit}

case class UMLCollector(plantUMLUnit:UMLUnit)

object UMLCollector {
  def apply(source: Source): UMLCollector = {
    val topLevelElements = StatsCollector(source.stats)(CollectorContext())
    new UMLCollector(uml.UMLUnit("need_to_find_id",toplevelElements = topLevelElements.definedElements.asInstanceOf[List[TopLevelElement]]))
  }
}
