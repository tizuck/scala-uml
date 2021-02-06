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

import uml.{AccessModifier, Private, Protected}

import scala.meta.Mod

case class AccessModifierCollector(accessModifier : Option[AccessModifier])

object AccessModifierCollector {
  def apply(mods:List[Mod]): AccessModifierCollector = {
    val accessMods = for(mod <- mods) yield {
      mod match {
        case Mod.Private(_) =>
          List(Private)
        case Mod.Protected(_) =>
          List(Protected)
        case _ => Nil
      }
    }

    //Multiple Access modifier are not allowed
    new AccessModifierCollector(accessMods.flatten.foldLeft[Option[AccessModifier]](None){
      case (Some(m), _) => Some(m)
      case (None,amod) => Some(amod)
      case _ => None
    })

  }
}
