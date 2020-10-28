package scalameta

import scala.meta.{Defn, Source}
import plantuml._
import scalameta.toplevel.DefnToplevelCollector
import uml.{TopLevelElement, UMLUnit}

case class PlantUMLCollector(plantUMLUnit:UMLUnit)

object PlantUMLCollector {
  def apply(source: Source): PlantUMLCollector = {
    val collectedDefs = source.stats.foldLeft[(List[DefnToplevelCollector],CollectorContext)]((Nil,CollectorContext())){
      case (acc,defn:Defn) =>
        val defnCol = DefnToplevelCollector(defn)(acc._2)
        (defnCol :: acc._1,acc._2 + defnCol.resultingContext)
      case (acc,_) => acc
    }

    val defnToplevelCollectors = collectedDefs._1
    //val defnContext = collectedDefs._2

    val topLevelUMLElements = defnToplevelCollectors.flatMap {tdc => tdc.topLevelElement :: tdc.relationships ++ tdc.typeClasses}
    new PlantUMLCollector(uml.UMLUnit("need_to_find_id",toplevelElements = topLevelUMLElements))
  }
}
