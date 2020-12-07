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

package scalameta.stats.defn

import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.{Attribute, Relationship, TaggedValue, UMLElement}

import scala.meta.Defn

case class DefnOpaqueTypealiasCollector(override val definedElements: List[UMLElement],
                                        override val resultingContext: CollectorContext)
  extends BaseCollector

object DefnOpaqueTypealiasCollector {
  def apply(opType:Defn.OpaqueTypeAlias)(implicit context:CollectorContext):DefnOpaqueTypealiasCollector = {
    val defnType = DefnTypeCollector(Defn.Type(opType.mods,opType.name,opType.tparams,opType.body))
    val typeFromCollector: List[uml.Class] = defnType.definedElements.flatMap{
      case c:uml.Class => Some(c)
      case _ => None
    }
    
    val relationship: List[Relationship] = defnType.definedElements.flatMap{
      case r:Relationship => Some(r)
      case _ => None
    }
    //@todo find out what opType.bounds on opaque types are
    DefnOpaqueTypealiasCollector(
      typeFromCollector
        .map( c =>
          c.copy(
            additionalCompartements =
              List(uml.Compartment(Some("<<ScalaClass>>"),List(TaggedValue("isOpaque",None)),Nil))
          )) ++ relationship,
        context
    )
  }
}
