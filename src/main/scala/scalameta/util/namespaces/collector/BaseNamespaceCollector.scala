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

import scalameta.util.namespaces.{Entry, NamespaceEntry}

import scala.meta.{Defn, Pkg, Stat, Term}

trait BaseNamespaceCollector {

  val resultingMap : scala.collection.immutable.Map[Entry,List[Stat]]

}

object BaseNamespaceCollector {
  /**
   * builds the package name
   * @param qual
   * @return
   */
  def qualName(qual:Term.Ref): NamespaceEntry = qual match {
    case Term.Name(str) => NamespaceEntry(List(str))
    case Term.Select(s:Term.Select,Term.Name(str)) => qualName(s).append(str)
    case Term.Select(n1:Term.Name,n2:Term.Name) => NamespaceEntry(n1.value :: n2.value :: Nil)
    case _ => throw new IllegalStateException(s"unexpected Package name:$qual")
  }
}
