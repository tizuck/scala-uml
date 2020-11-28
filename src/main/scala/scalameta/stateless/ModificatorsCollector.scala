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
import uml.{Modificator, Static}

import scala.meta.Mod

case class ModificatorsCollector(modificators:List[Modificator])

object ModificatorsCollector {
  def apply(mods:List[Mod])(implicit context : CollectorContext): ModificatorsCollector = {
    val modificators = for (mod <- mods) yield {
      mod match {
        case Mod.Final() => List(Static)
        case Mod.Abstract() => List(Static)
        case _ => Nil
      }
    }

    new ModificatorsCollector(modificators.flatten)
  }
}