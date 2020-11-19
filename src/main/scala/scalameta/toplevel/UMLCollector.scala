package scalameta

import scalameta.stats.StatsCollector
import scalameta.util.context.{CollectorContext, GlobalContext}

import scala.meta.{Defn, Source}
import uml.{TopLevelElement, UMLUnit}

case class UMLCollector(plantUMLUnit:UMLUnit,resultingContext:CollectorContext)

object UMLCollector {
  //@todo Add the name of the file additionally to the source so the find
  //  algorithm can respect current compilation unit
  def apply(source: Source,pre:GlobalContext,compilationUnit:String): UMLCollector = {
    val topLevelElements = StatsCollector(source.stats)(CollectorContext(compilationUnit,pre))
    println("toplevel defined elements: " + topLevelElements.definedElements.map(_.structure))
    //Substitute all external references that are not defined in the current compilation unit
    val externals = topLevelElements.resultingContext.localCon.externalReferences
    val externalsToInclude = for {e <- externals} yield {
      val toplevelElements = topLevelElements.definedElements
      Option.when(toplevelElements.find{
        case c:uml.Class => c.identifier.equals(e.name) && c.namespace.equals(e.namespace)
        case _ => false
      }.isEmpty)(e)
    }
    new UMLCollector(
      uml.UMLUnit(
        "need_to_find_id",
        toplevelElements =
          topLevelElements.definedElements.asInstanceOf[List[TopLevelElement]].distinct ++
            externalsToInclude.flatten
      ),topLevelElements.resultingContext)
  }
}
