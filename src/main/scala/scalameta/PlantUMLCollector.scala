package scalameta

import scala.meta.{Defn, Source}
import plantuml._
import scalameta.toplevel.ToplevelDefnCollector

case class PlantUMLCollector(plantUMLUnit:PlantUMLUnit)

object PlantUMLCollector {
  def apply(source: Source): PlantUMLCollector = {
    val collected:List[ToplevelDefnCollector] =
      for (stat <- source.stats) yield {
        stat match {
          case defn: Defn =>
            ToplevelDefnCollector(defn)(CollectorContext(thisPointer = ""))
        }
      }

    val topLevelUMLElements = collected.flatMap {tdc => tdc.topLevelElement :: tdc.relationships}
    new PlantUMLCollector(PlantUMLUnit("need_to_find_id",toplevelElements = topLevelUMLElements))
  }
}
