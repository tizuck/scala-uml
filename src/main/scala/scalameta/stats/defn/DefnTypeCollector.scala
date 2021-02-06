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
    val generics = TypeParamsCollector(dType.tparams)

    val typeRep = TypeNameCollector(dType.body)
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

    //If the type is defined within another entity
    if(context.localCon.thisPointer.isDefined){
      val relationshipInfo = RelationshipInfo(None,None,context.localCon.thisPointer.get,ConcreteClass(typeClass),None,Without,originType = context.localCon.thisOriginType)
      val relationship = Relationship(Inner,ToFrom,relationshipInfo,Nil)
      new DefnTypeCollector(relationship :: typeClass :: Nil,context)
    }//if type is defined on toplevel
    else {
      new DefnTypeCollector(typeClass :: Nil,context)
    }
  }
}
