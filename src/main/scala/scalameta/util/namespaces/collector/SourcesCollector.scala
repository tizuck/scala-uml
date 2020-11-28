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
import scalameta.util.namespaces.{DefaultNamespace, Entry, NamespaceEmpty, NamespaceEntry}

import scala.meta.{Pkg, Source, Stat, Term}
import cats.implicits._
import scalameta.util.util.statToString

case class SourcesCollector(resultingMap: Map[Entry, List[(Stat,String)]])

object SourcesCollector {
  def apply(sources:List[(Source,String)]): SourcesCollector = {
    SourcesCollector(sources.foldLeft(Map.empty[Entry,List[(Stat,String)]]){
      case (acc,source) =>
        val sourceCollector = SourceCollector(source._1)(source._2)
        val sourceMap =
          sourceCollector
            .resultingMap
            //All elements that result with a `NamespaceEmpty` entry on toplevel are positioned in the default package
            .map(tp => tp._1 match {case NamespaceEntry(List("default"),_) => DefaultNamespace -> tp._2 case _ => tp})
            //Need new map that only contains NamespaceEmpty entries and merge them with a foldleft to a map that only contains
            //DefaultNamespace -> ...
        //Get a map with only the element that has key NamespaceEmpty
        val onlyNamespaceEmpty = sourceMap.filter(pred => pred._1 match {case NamespaceEmpty => true case _ => false})
        //map that entry to DefaultNamespace
        val defaultsOfNamespaceEmpty = onlyNamespaceEmpty.foldLeft(Map[Entry,List[(Stat,String)]]()){
          case (acc,entry) => acc |+| Map(entry).map(tp => tp._1 match {case NamespaceEmpty => DefaultNamespace -> tp._2 case _ => tp})
        }
        //Filter the old changed key in sourceMap that has key NamespaceEmpty and merge it with the new key
        val beforeMerge = sourceMap
          .filterNot(pred => pred._1 match {case NamespaceEmpty => true case _ => false}) |+| defaultsOfNamespaceEmpty
        //Filter algorithmic redundant entry
          .map(tp =>
            (tp._1,tp._2.filterNot(p => p._1 match {case Pkg(Term.Name("default"),_) => true case _ => false})))

        //merge accumulator with new map of current source
        val after = acc |+| beforeMerge
        //Filter multiple elements on toplevel (Could occur due to sources that define elements in the same namespace)
        after.map(tp => (tp._1,tp._2.distinct))
    }
    )
  }
}
