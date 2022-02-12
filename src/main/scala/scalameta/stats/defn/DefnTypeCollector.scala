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

import scalameta.stateless.TypeNameCollector
import scalameta.typeparams.TypeParamsCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml._

import scala.meta.Defn

case class DefnTypeCollector(override val definedElements: List[UMLElement],
                             override val resultingContext: CollectorContext)
  extends BaseCollector

object DefnTypeCollector {
  def apply(dType:Defn.Type)(implicit context:CollectorContext): DefnTypeCollector = {

    val typeRep = TypeNameCollector(dType.body)

    if(context.localCon.isTopLevel){
      val generics = TypeParamsCollector(dType.tparams)
      //@todo add a proper type here
      val attr = Attribute(None,None,"_type_",Some(typeRep.typeRep),List(Stereotype("typeDef",Nil)))
      val typeClass = Class(
        isAbstract = true,
        dType.name.value,
        List(attr),
        Nil,
        Nil,
        Option.when(generics.typeParams.nonEmpty)(generics.typeParams),
        List(Stereotype("type",Nil)),
        context.localCon.currentNamespace)

      new DefnTypeCollector(typeClass :: Nil,context)

    } else {
      val attribute = Attribute(None,None,dType.name.value,Some(typeRep.typeRep),List(Stereotype("type",Nil)),None)

      new DefnTypeCollector(attribute :: Nil,context)

    }
  }
}
