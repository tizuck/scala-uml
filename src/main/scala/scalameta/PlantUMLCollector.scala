package scalameta

import scala.meta.{Defn, Source}
import plantuml._
import scalameta.toplevel.DefnToplevelCollector

case class PlantUMLCollector(plantUMLUnit:PlantUMLUnit)

object PlantUMLCollector {
  def apply(source: Source): PlantUMLCollector = {
    val collected:List[DefnToplevelCollector] =
      for (stat <- source.stats) yield {
        stat match {
          case defn: Defn =>
            DefnToplevelCollector(defn)(CollectorContext(thisPointer = ""))
        }
      }

    val topLevelUMLElements = collected.flatMap {tdc => tdc.topLevelElement :: tdc.relationships}
    new PlantUMLCollector(PlantUMLUnit("need_to_find_id",toplevelElements = topLevelUMLElements))
  }
}
