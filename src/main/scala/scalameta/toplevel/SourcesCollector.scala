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

package scalameta.toplevel

import scalameta.implicits._
import cats.implicits._
import scalameta.SourceCollector
import scalameta.util.context.{CollectorContext, GlobalContext}
import uml.UMLUnit

import scala.meta.Source

case class SourcesCollector(umlUnit:UMLUnit) {

}

object SourcesCollector {
  def apply(sources:List[(Source,String)],identifier:String): SourcesCollector = {
    //create global index
    val namespacing =
      scalameta
        .util
        .namespaces
        .collector
        .SourcesCollector(sources).resultingMap

    val res = sources
      //iterate through all source files and collect umlUnits
      //then merge them together using the semigroup property
      //of SourceCollector
      .foldLeft(Option.empty[SourceCollector]){
        case (acc,(source,compUnit)) =>
          val sourceCol = SourceCollector(source,GlobalContext(namespacing),compUnit)
          acc
          .map(s => s |+| sourceCol)
          .orElse(Some(sourceCol))
      }
      //All entities that are not defined in the collection of sources
      //are inserted from the externalReferences field.
      .map{ finalS =>
        val externals = finalS.resultingContext.localCon.externalReferences
        val externalsToInclude = for {e <- externals} yield {
          val topLevelElement = finalS.umlUnit.toplevelElements
          Option.when(!topLevelElement.exists{
            case c: uml.Class => c.name.equals(e.name) && c.namespace.equals(e.namespace)
            case _ => false
          })(e)
        }
        finalS.copy(umlUnit = finalS.umlUnit.copy(toplevelElements = finalS.umlUnit.toplevelElements ++ externalsToInclude.flatten))
      }
    SourcesCollector(res.map(s => s.umlUnit).getOrElse(UMLUnit("",Nil)))
  }
}
