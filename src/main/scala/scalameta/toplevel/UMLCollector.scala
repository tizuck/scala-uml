package scalameta

import scala.meta.{Defn, Source}
import uml.{TopLevelElement, UMLUnit}

case class PlantUMLCollector(plantUMLUnit:UMLUnit)

object PlantUMLCollector {
  def apply(source: Source): PlantUMLCollector = {
    val topLevelElements = StatsCollector(source.stats)(CollectorContext(None,Nil,None))
    new PlantUMLCollector(uml.UMLUnit("need_to_find_id",toplevelElements = topLevelElements.definedElements.asInstanceOf[List[TopLevelElement]]))
  }
}
