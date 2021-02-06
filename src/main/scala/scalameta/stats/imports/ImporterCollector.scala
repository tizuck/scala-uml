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

import scalameta.util.namespaces.{Name, NamespaceEntry, Wildcard}

import scala.meta.Importer

case class ImporterCollector(namespaces:List[NamespaceEntry])

object ImporterCollector {
  def apply(importer:Importer): ImporterCollector = {
    val importees = ImporteesCollector(importer.importees).names
    val importNamespace = ImportRefCollector(importer.ref).namesspace
    ImporterCollector(importees.foldLeft(List.empty[NamespaceEntry]){
      case (acc,name) =>
        if(!name.isEmpty){
          acc ++ List(importNamespace.append(name).copy(targetType = Name))
        } else {
          acc ++ List(importNamespace.copy(targetType = Wildcard))
        }
    })
  }
}
