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
import scalameta.util.namespaces.Entry

import scala.meta.{Defn, Pkg, Stat}

case class StatCollector(override val resultingMap: Map[Entry, List[Stat]])
  extends BaseNamespaceCollector

object StatCollector {
  def apply(stat:Stat): StatCollector = stat match {
    case pkg:Pkg => StatCollector(PkgCollector(pkg).resultingMap)
    case cls:Defn.Class =>StatCollector(ClassCollector(cls).resultingMap)
    case enum:Defn.Enum => StatCollector(EnumCollector(enum).resultingMap)
    case obj:Defn.Object => StatCollector(ObjectCollector(obj).resultingMap)
    case trt:Defn.Trait => StatCollector(TraitCollector(trt).resultingMap)
    case tpe:Defn.Type => StatCollector(TypeCollector(tpe).resultingMap)
    case _ => StatCollector(Map.empty[Entry,List[Stat]])
   }
}
