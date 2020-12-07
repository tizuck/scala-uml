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

import scalameta.util.namespaces.NamespaceEntry

import scala.meta.Term

case class ImportRefCollector(namesspace:NamespaceEntry)

object ImportRefCollector {
  def apply(ref:Term.Ref): ImportRefCollector = ref match {
    case Term.Select(t:Term.Select,n:Term.Name) =>
      ImportRefCollector(ImportRefCollector(t).namesspace.append(n.value))
    case Term.Select(t1:Term.Name,t2:Term.Name) =>
      ImportRefCollector(NamespaceEntry(List(t1.value,t2.value)))
    case Term.Name(s) => ImportRefCollector(NamespaceEntry(List(s)))
  }
}
