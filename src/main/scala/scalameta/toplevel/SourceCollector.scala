/*
 * Copyright 2015 Tilman Zuckmantel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
