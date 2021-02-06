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

package scalameta.stats.dcl

import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml.UMLElement

import scala.meta.Decl

case class DclCollector(definedElements:List[UMLElement],
                        override val resultingContext: CollectorContext) extends BaseCollector

object DclCollector {
  def apply(dcl:Decl)(implicit context: CollectorContext): DclCollector = {
    val dclRelationsships = dcl match {
      case d : Decl.Val => Some(DclValCollector(d))
      case d : Decl.Var => Some(DclVarCollector(d))
      case d : Decl.Def => Some(DclDefCollector(d))
      case d : Decl.Type => Some(DclTypeCollector(d))
      case _ => None
    }

    val wrap = dclRelationsships.getOrElse(new BaseCollector {
      override val definedElements: List[UMLElement] = Nil
      override val resultingContext: CollectorContext = context
    })


    new DclCollector(wrap.definedElements,wrap.resultingContext)
  }
}
