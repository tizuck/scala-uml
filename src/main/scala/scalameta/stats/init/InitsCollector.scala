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

package scalameta.stats.init

import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.UMLElement

import scala.meta.Init

case class InitsCollector(override val resultingContext: CollectorContext,
                          override val definedElements: List[UMLElement]) extends BaseCollector

object InitsCollector {
  def apply(inits:List[Init])(implicit context : CollectorContext): InitsCollector =  {
      inits.foldLeft(InitsCollector(context,Nil)){
        case (acc,init) =>
          val initCol = InitCollector(init)(acc.resultingContext)
          acc.copy(initCol.resultingContext,definedElements = acc.definedElements ++ initCol.definedElements)
      }
  }

}
