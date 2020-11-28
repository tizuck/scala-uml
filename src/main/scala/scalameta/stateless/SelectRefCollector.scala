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

package scalameta.stateless

import scalameta.util.namespaces.NamespaceEntry

import scala.meta.Term

case class SelectRefCollector(namespaceAddition:NamespaceEntry)

object SelectRefCollector {
  def apply(ref:Term.Ref): SelectRefCollector = SelectRefCollector( ref match {
    case Term.Name(str) => NamespaceEntry(List(str))
    case Term.Select(Term.Name(n), name) => NamespaceEntry(List(n,name.value))
    case Term.Select(s:Term.Select,name) => this(s).namespaceAddition.append(name.value)
  })
}
