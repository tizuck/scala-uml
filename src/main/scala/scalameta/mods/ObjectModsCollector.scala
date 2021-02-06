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

package scalameta.mods

import scalameta.util.context.CollectorContext
import uml.{Compartment, Stereotype}

import scala.meta.Mod



case class ObjectModsCollector(modifiers:List[Compartment], objectStereotypes:List[Stereotype]) {

}

object ObjectModsCollector {
  def apply(mods:List[Mod])(implicit context:CollectorContext): ObjectModsCollector = {
    val classMods = ClassModsCollector(mods)
    val caseObjectStereotype =
      classMods
        .classStereotypes
        .map( s =>
        if(s.equals(Stereotype("caseclass",Nil))){Stereotype("caseobject",Nil)}
        else throw new IllegalArgumentException(s"unexpected stereotype: $s")
      )
    new ObjectModsCollector(classMods.mods,caseObjectStereotype)
  }
}
