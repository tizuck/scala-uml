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
import scalameta.util.context.{CollectorContext, GlobalContext, LocalContext}
import scalameta.util.namespaces.DefaultNamespace
import uml.UMLUnit

object implicits {

  implicit val sourceCollectorSemiGroup:Semigroup[SourceCollector] = (x: SourceCollector, y: SourceCollector) => {
    val xToplevel = x.umlUnit.toplevelElements
    val yToplevel = y.umlUnit.toplevelElements

    val xResultingExternals = x.resultingContext.localCon.externalReferences
    val yResultingExternals = y.resultingContext.localCon.externalReferences

    SourceCollector(UMLUnit(x.umlUnit.name, xToplevel |+| yToplevel), x.resultingContext |+| y.resultingContext)
  }

  /**
   * @todo not associative but for the purpose it is not needed
   */
  implicit val collectorContextSemiGroup:Semigroup[CollectorContext] = (x: CollectorContext, y: CollectorContext) => {
    CollectorContext(
      LocalContext(
        None,
        "",
        None,
        x.localCon.currentImports |+| y.localCon.currentImports,
        DefaultNamespace,
        true,
        true,
        x.localCon.externalReferences |+| y.localCon.externalReferences,
        x.localCon.opReps
      ),
      GlobalContext(
        x.globalCon.globalScope |+| y.globalCon.globalScope
      )
    )
  }
}
