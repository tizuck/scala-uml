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

package scalameta.util.namespaces.collector
import scalameta.util.namespaces.{Entry, NamespaceEmpty}

import scala.meta.{Defn, Stat, Term}


case class ObjectCollector(override val resultingMap: Map[Entry, List[Stat]])
  extends BaseNamespaceCollector

object ObjectCollector {
  def apply(obj:Defn.Object): ObjectCollector = {
    val statsNamespaces = StatsCollector(
      obj.templ.stats,Some(BaseNamespaceCollector.qualName(Term.Name(obj.name.value)))
    )
    ObjectCollector(
      statsNamespaces.resultingMap + (NamespaceEmpty -> List(obj))
    )
  }
}
