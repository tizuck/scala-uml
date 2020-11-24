package scalameta

import cats.kernel.Semigroup
import cats.implicits._

import scalameta.stats.StatsCollector
import scalameta.util.context.{CollectorContext, GlobalContext}

import scala.meta.{Defn, Source}
import uml.{TopLevelElement, UMLUnit}

/**
 * Collects all uml elements that are defined in a `scala.meta.Source`.
 *
 * Resulting `umlUnit` contains unresolved `ClassDefRef`-instances for further
 * processing in the `SourcesCollector`.
 *
 * Extends the Semigroup trait of cats to enable associative combining of
 * multiple sources. External references are added and resolved after combining.
 *
 * @param umlUnit
 * @param resultingContext
 */
case class SourceCollector(umlUnit:UMLUnit, resultingContext:CollectorContext) {

}

object SourceCollector {
  //@todo Add the name of the file additionally to the source so the find
  //  algorithm can respect current compilation unit
  def apply(source: Source,pre:GlobalContext,compilationUnit:String): SourceCollector = {
    val topLevelElements = StatsCollector(source.stats)(CollectorContext(compilationUnit,pre))

    new SourceCollector(
      uml.UMLUnit(
        "need_to_find_id",
        toplevelElements =
          topLevelElements.definedElements.asInstanceOf[List[TopLevelElement]].distinct
      ),topLevelElements.resultingContext)
  }
}
