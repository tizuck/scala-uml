package scalameta

import scalameta.stats.StatsCollector
import scalameta.util.context.{CollectorContext, GlobalContext}

import scala.meta.{Defn, Source}
import uml.{TopLevelElement, UMLUnit}

case class UMLCollector(plantUMLUnit:UMLUnit,resultingContext:CollectorContext)

object UMLCollector {
  //@todo Add the name of the file additionally to the source so the find
  //  algorithm can respect current compilation unit
  def apply(source: Source,pre:GlobalContext): UMLCollector = {
    val topLevelElements = StatsCollector(source.stats)(CollectorContext(pre))
    new UMLCollector(
      uml.UMLUnit(
        "need_to_find_id",
        toplevelElements = topLevelElements.definedElements.asInstanceOf[List[TopLevelElement]].distinct
      ),topLevelElements.resultingContext)
  }
}
