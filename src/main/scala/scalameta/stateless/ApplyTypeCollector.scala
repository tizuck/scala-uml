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

import scalameta.util.context.CollectorContext
import scalameta.util.namespaces.Entry

import scala.meta.{Stat, Type}


case class ApplyTypeCollector(namespace:Entry,target:String,oStat:Option[Stat])

object ApplyTypeCollector {
  def apply(tpe:Type)(implicit context:CollectorContext): ApplyTypeCollector = tpe match {
    case Type.Name(name) =>
      val lookup = context
        .globalCon
        .find(
          name,
          None,
          context.localCon.currentCompilationUnit,
          context.localCon.currentNamespace,
          context.localCon.lastPackageNamespace,
          context.localCon.currentImports)

      new ApplyTypeCollector(
        lookup.map(_._1).getOrElse(context.localCon.currentNamespace),
        name,
        lookup.flatMap(_._2))

    case Type.Select(qual,name) =>
      val qualResolved = SelectRefCollector(qual)

      val lookup = context
        .globalCon
        .find(
          name.value,
          Some(qualResolved.namespaceAddition),
          context.localCon.currentCompilationUnit,
          context.localCon.currentNamespace,
          context.localCon.lastPackageNamespace,
          context.localCon.currentImports
        )

      new ApplyTypeCollector(
        lookup.map(_._1).getOrElse(context.localCon.currentNamespace),
        name.value,
        lookup.flatMap(_._2)
      )
  }
}
