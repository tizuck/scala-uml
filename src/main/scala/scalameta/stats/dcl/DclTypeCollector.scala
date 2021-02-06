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
import scalameta.typeparams.TypeParamsCollector
import scalameta.util.BaseCollector
import scalameta.util.context.CollectorContext
import uml._

import scala.meta.Decl

case class DclTypeCollector(override val definedElements: List[UMLElement]
                            , override val resultingContext: CollectorContext
                                        )
  extends BaseCollector

object DclTypeCollector {
  def apply(dclType:Decl.Type)(implicit context : CollectorContext): DclTypeCollector = {
    //@todo types can have generic parameters
    val generics = TypeParamsCollector(dclType.tparams)

    val typeClass = Class(
      isAbstract = true,
      dclType.name.value,
      Nil,
      Nil,
      Nil,
      Option.when(generics.typeParams.nonEmpty)(generics.typeParams),
      List(Stereotype("type",Nil)),
      context.localCon.currentNamespace)

    //If the type is defined within another entity
    if(context.localCon.thisPointer.isDefined){
      val relationshipInfo = RelationshipInfo(None,None,context.localCon.thisPointer.get,ConcreteClass(typeClass),None,Without)
      val relationship = Relationship(Inner,ToFrom,relationshipInfo,Nil)
      new DclTypeCollector(relationship :: typeClass :: Nil,context)
    }//if type is defined on toplevel
    else {
      new DclTypeCollector(typeClass :: Nil,context)
    }
  }
}
