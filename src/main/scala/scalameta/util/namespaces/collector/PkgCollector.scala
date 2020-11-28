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

import scalameta.util.namespaces.{Entry, NamespaceEmpty, NamespaceEntry, collector}
import scalameta.util.util.statToString

import scala.meta.{Defn, Pkg, Stat}

case class PkgCollector(override val resultingMap: Map[Entry, List[Stat]])
  extends BaseNamespaceCollector

object PkgCollector {
  def apply(pkg:Pkg): PkgCollector = {
    val pkgNamespace = BaseNamespaceCollector.qualName(pkg.ref)
    val statsNamespaces = StatsCollector(pkg.stats,Some(pkgNamespace)).resultingMap
    PkgCollector(statsNamespaces + (NamespaceEmpty -> List(pkg)))
  }
}
