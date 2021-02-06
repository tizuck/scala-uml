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

package scalameta.stats

import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import scalameta.util.namespaces.collector.BaseNamespaceCollector.qualName
import scalameta.util.namespaces.{DefaultNamespace, NamespaceEntry}
import uml.UMLElement

import scala.meta.Pkg

case class PkgCollector(override val resultingContext: CollectorContext,
                        override val definedElements: List[UMLElement])
  extends BaseCollector

object PkgCollector {
  /**
   * Updates the current namespace of the local context and sets it
   * according to the package reference. Visits all nodes contained
   * in `pkg.stats` with the updated namespace
   *
   * @param pkg visited node that contains namespace information
   * @param context context of previous node visiting
   * @return all collected elements of `pkg.stats` in `definedElements` and
   *         a context with the previous namespace and
   *         updated local context information
   */
  def apply(pkg: Pkg)(implicit context: CollectorContext): PkgCollector = {
    val oldNamespace = context.localCon.currentNamespace
    val newNamespace = oldNamespace match {
      case DefaultNamespace => qualName(pkg.ref)
      case NamespaceEntry(qualifiers,_) => NamespaceEntry(qualifiers ++ qualName(pkg.ref).qualifiers)
      case _ => throw new IllegalStateException(s"Unexpected empty namespace in pkg: ${pkg.ref}")
    }
    val innerstats = StatsCollector(pkg.stats)(context.withNamespace(newNamespace))

    PkgCollector(
      innerstats
        .resultingContext
        .withNamespace(oldNamespace)
        .withLastPackageNamespace(newNamespace),
      innerstats.definedElements
    )
  }
}

