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

package scalameta.stats.imports

import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import scalameta.util.namespaces.NamespaceEntry
import uml.UMLElement

import scala.meta.Import

case class ImportCollector(override val definedElements: List[UMLElement],
                           override val resultingContext: CollectorContext) extends BaseCollector

object ImportCollector {
  def apply(imprt:Import)(implicit context:CollectorContext): ImportCollector = {
    val namespaces = imprt.importers.foldLeft(List.empty[NamespaceEntry]){
      case (acc,importer) => acc ++ ImporterCollector(importer).namespaces
    }
    ImportCollector(Nil,context.withAdditionalImports(namespaces))
  }
}
